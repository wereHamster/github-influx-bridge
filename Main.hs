{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Safe

import           Snap.Core
import           Snap.Http.Server hiding (Config)

import           Data.Aeson (decode)
import           Data.Aeson.Types (parseMaybe)

import qualified Data.Text as T
import           Data.Text.Encoding

import qualified Data.Vector as V

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


influxConfig :: Manager -> IO (Maybe Config)
influxConfig httpManager = runMaybeT $ do
    uri <- MaybeT $ lookupEnv "INFLUXDB"
    URI{..} <- MaybeT $ return $ parseURI uri
    URIAuth{..} <- MaybeT $ return $ uriAuthority
    (username, password) <- MaybeT $ case T.splitOn ":" (T.pack $ init uriUserInfo) of
        [a,b] -> return $ Just (a,b)
        _     -> return Nothing


    port <- MaybeT $ return $ readMay (tail uriPort)
    serverPool <- MaybeT $ Just <$> newServerPool
        (Server (T.pack uriRegName) port False)
        []


    config <- MaybeT $ return $ Just $ Config
        (Credentials username password)
        serverPool
        httpManager


    MaybeT $ return $ Just config



main :: IO ()
main = do
    withManager managerSettings $ \manager -> do
        mbConfig <- influxConfig manager
        case mbConfig of
            Nothing -> return ()
            Just cfg -> quickHttpServe $
                path "webhook" (method POST $ hook cfg) <|> writeText "ok\n"


hook :: Config -> Snap ()
hook config = do
    mbEvent <- do
        hdrs <- headers <$> getRequest
        body <- readRequestBody (100 * 1000)

        return $ do
            eventName <- getHeader "X-GitHub-Event" hdrs
            value     <- decode body
            parseMaybe (eventParser $ decodeUtf8 eventName) value

    case mbEvent of
        Nothing -> return ()
        Just ev -> do
            void $ liftIO $ postWithPrecision config "rmx" SecondsPrecision $
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
