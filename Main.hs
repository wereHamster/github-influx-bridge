{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Safe

import           Snap.Core
import           Snap.Http.Server hiding (Config)

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Vector as V

import           Data.Maybe
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
import           GitHub.WebHook.Handler
import           GitHub.WebHook.Handler.Snap

import           Prelude



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
                webhookHandler (BC8.pack hookPath) mbSecretKey (handleEvent cfg db) <|> writeText "ok\n"

handleEvent :: Config -> Text -> Either Error Event -> Snap ()
handleEvent config db res = case res of
    Left _ -> do
        writeText "not ok\n"

    Right ev -> do
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
