{-# LANGUAGE LambdaCase #-}

module Dampf.Postgres where

import           Control.Lens
import           Control.Monad              (when)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Text                  (Text)
import qualified Data.Text as T
import           System.Process.Typed

import           Dampf.Postgres.Connect
import           Dampf.Postgres.Migrate
import           Dampf.Postgres.Setup
import           Dampf.Types


runMigrations :: (MonadIO m, MonadThrow m) => Maybe Text -> DampfT m ()
runMigrations mdb = do
    ds <- view (app . databases)

    iforM_ ds $ \name spec ->
        when (maybe True (== name) mdb) $ migrate name spec


newMigrationCmd :: (MonadIO m, MonadThrow m)
    => Text -> FilePath -> DampfT m ()
newMigrationCmd name mig = view (app . databases . at name) >>= \case
    Just spec -> liftIO $ newMigration mig spec
    Nothing   -> throwM $ InvalidDatabase name


setupDB :: (MonadIO m, MonadThrow m) => DampfT m ()
setupDB = do
    createUsers
    createDatabases
    createExtensions


backupDB :: (MonadIO m, MonadThrow m) => Maybe Text -> DampfT m ()
backupDB mdb = do
    ms <- view (config . databaseServer)
    ds <- view (app . databases)

    case ms of
        Just s  -> iforM_ ds $ \name spec ->
            when (maybe True (== name) mdb) $ do
                let fileName = "backup_" ++ T.unpack name ++ ".sqlc"
                let passwd   = lookupPassword (spec ^. user) s
                let envs     = [("PGDATABASE", T.unpack name)
                                , ("PGUSER", spec ^. user)
                                , ("PGPASSWORD", passwd)
                                ]

                let cmd  = setEnv envs $ shell $ "pg_dump -Fc >" ++ fileName

                runProcess_ cmd

        Nothing -> throwM NoDatabaseServer
            
--PGPASSWORD=mypassword pg_dump -Fc -U myuser filocore >../db_dump

