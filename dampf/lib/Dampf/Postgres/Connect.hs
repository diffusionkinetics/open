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

import           Dampf.Types


lookupPassword :: (HasPostgresConfig c) => Text -> c -> String
lookupPassword name cfg = case cfg ^. users . at name of
    Nothing -> error $ "no password for user "++ T.unpack name ++ " in .dampf.cfg"
    Just pw -> T.unpack pw


createSuperUserConn :: (MonadIO m, MonadThrow m)
    => Text -> DampfT m Connection
createSuperUserConn name = createConn name spec
  where
    spec = DatabaseSpec Nothing "postgres" []

createSuperUserPostgresConn :: (MonadIO m, MonadThrow m)
    => DampfT m Connection
createSuperUserPostgresConn = createConn "postgres" spec
  where
    spec = DatabaseSpec Nothing "postgres" []

createConn :: (MonadIO m, MonadThrow m)
    => Text -> DatabaseSpec -> DampfT m Connection
createConn name spec = view (config . postgres) >>= \case
    Just s  -> liftIO $ connect ConnectInfo
        { connectHost       = s ^. host . to T.unpack
        , connectPort       = s ^. port . to fromIntegral
        , connectUser       = spec ^. user . to T.unpack
        , connectPassword   = lookupPassword (spec ^. user) s
        , connectDatabase   = T.unpack name
        }

    Nothing -> throwM NoDatabaseServer


destroyConn :: (MonadIO m) => Connection -> DampfT m ()
destroyConn = liftIO . close

