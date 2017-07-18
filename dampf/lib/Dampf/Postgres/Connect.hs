{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dampf.Postgres.Connect where

import           Control.Lens
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Text                  (Text)
import qualified Data.Text as T
import           Database.PostgreSQL.Simple

import           Dampf.AppFile
import           Dampf.ConfigFile
import           Dampf.Types


lookupPassword :: (HasPostgresConfig c) => String -> c -> String
lookupPassword name cfg = case cfg ^. users . at (T.pack name) of
    Nothing -> error $ "no password for user "++ name ++ " in .dampf.cfg"
    Just pw -> T.unpack pw


createSuperUserConn :: (MonadIO m, MonadThrow m)
    => Text -> Text -> DampfT m Connection
createSuperUserConn server name = createConn server name spec
  where
    spec = DatabaseSpec Nothing "postgres" []


createConn :: (MonadIO m, MonadThrow m)
    => Text -> Text -> DatabaseSpec -> DampfT m Connection
createConn server name spec =
    view (config . databaseServers . at server) >>= \case
        Just c  -> liftIO $ connect ConnectInfo
            { connectHost     = c ^. host
            , connectUser     = spec ^. user
            , connectPassword = lookupPassword (spec ^. user) c
            , connectDatabase = T.unpack name
            , connectPort     = c ^. port ^. to fromIntegral
            }

        Nothing -> throwM $ InvalidDatabase name


destroyConn :: (MonadIO m) => Connection -> DampfT m ()
destroyConn = liftIO . close

