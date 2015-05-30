{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Safe

import           Snap.Core
import           Snap.Http.Server hiding (Config)

import           Crypto.Hash

import           Data.Aeson (decode)
import           Data.Aeson.Types (parseMaybe)

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Monoid

import qualified Data.Vector as V

import           Data.Maybe
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Char8 as BC8

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe

import           Network.URI hiding (path)
import           Network.Connection
import           Network.HTTP.Client hiding (path, method)
import           Network.HTTP.Client.TLS

import           System.Environment

import           Database.InfluxDB

import           GitHub.Types



-- | Need to disable certificate verification because it is broken.
managerSettings :: ManagerSettings
managerSettings = mkManagerSettings
    (TLSSettingsSimple True True True) Nothing


influxConfig :: Manager -> IO (Maybe (Config, Text))
influxConfig httpManager = runMaybeT $ do
    uri <- MaybeT $ lookupEnv "INFLUXDB"
    URI{..} <- MaybeT $ return $ parseURI uri
    URIAuth{..} <- MaybeT $ return $ uriAuthority
    (username, password) <- MaybeT $ case T.splitOn ":" (T.pack $ init uriUserInfo) of
        [a,b] -> return $ Just (a,b)
        _     -> return Nothing

    port <- MaybeT $ return $ Just $ case uriPort of
        (':':x) -> fromMaybe 8086 $ readMay x
        _ -> 8086

    serverPool <- MaybeT $ Just <$> newServerPool
        (Server (T.pack uriRegName) port False)
        []

    config <- MaybeT $ return $ Just $ Config
        (Credentials username password)
        serverPool
        httpManager

    -- Check to make sure the db is not an empty string.
    db <- MaybeT $ return $ case tail uriPath of
        [] -> Nothing
        x  -> Just x

    MaybeT $ return $ Just (config, T.pack db)



main :: IO ()
main = do
    hookPath    <- fromMaybe "webhook" <$> lookupEnv "HOOKPATH"
    mbSecretKey <- lookupEnv "SECRET_TOKEN"

    withManager managerSettings $ \manager -> do
        mbConfig <- influxConfig manager
        case mbConfig of
            Nothing -> return ()
            Just (cfg, db) -> quickHttpServe $
                path (BC8.pack hookPath) (method POST $ hook mbSecretKey cfg db) <|> writeText "ok\n"


hook :: Maybe String -> Config -> Text -> Snap ()
hook mbSecretKey config db = do
    mbEvent <- do
        hdrs <- headers <$> getRequest
        body <- do
            body <- readRequestBody (100 * 1000)
            case (mbSecretKey, getHeader "X-Hub-Signature" hdrs) of

                -- No secret key and no signature. Pass along the body unverified.
                (Nothing, Nothing) -> return body

                -- Signature is available but no secret key to verify it. Log this
                -- and accept the body.
                (Nothing, Just _) -> do
                    liftIO $ putStrLn $
                        "Ignoring signature because the secret token was not provided"
                    return body

                -- Secret token is available but the request is not signed. Reject
                -- the request.
                (Just _, Nothing) -> do
                    writeText "Signature missing"
                    res <- getResponse
                    finishWith $ setResponseCode 400 res

                -- Both the signature and secret token are available. Verify the
                -- signature and reject the request if that fails.
                (Just sc, Just sig) -> do
                    let mac = hmac (BC8.pack sc) (LBS.toStrict body) :: HMAC SHA1
                    if sig == ("sha1=" <> digestToHexByteString (hmacGetDigest mac))
                        then return body
                        else do
                            liftIO $ do
                                putStrLn "Signature does not match:"
                                print $ show sig
                                print $ digestToHexByteString $ hmacGetDigest mac

                            writeText "Signature does not match"
                            res <- getResponse
                            finishWith $ setResponseCode 400 res

        return $ do
            eventName <- getHeader "X-GitHub-Event" hdrs
            value     <- decode body
            parseMaybe (eventParser $ decodeUtf8 eventName) value

    case mbEvent of
        Nothing -> return ()
        Just ev -> do
            void $ liftIO $ postWithPrecision config db SecondsPrecision $
                withSeries "github.events" $ do
                    writePoints ev
            writeText "ok\n"



instance ToSeriesData Event where
    toSeriesColumns _ = V.fromList
        [ "type"
        , "repo"
        ]

    toSeriesPoints (CommitCommentEventType x) = V.fromList
        [ String "commit-comment"
        , String (repositoryFullName (commitCommentEventRepository x))
        ]

    toSeriesPoints (DeploymentEventType x) = V.fromList
        [ String "deployment"
        , String (repositoryFullName (deploymentEventRepository x))
        ]

    toSeriesPoints (DeploymentStatusEventType x) = V.fromList
        [ String "deployment-status"
        , String (repositoryFullName (deploymentStatusEventRepository x))
        ]
