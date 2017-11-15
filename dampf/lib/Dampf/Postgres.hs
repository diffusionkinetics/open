{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings  #-}

module Dampf.Postgres where

import           Control.Lens
import           Control.Monad              (when)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Text                  (Text)
import qualified Data.Text as T
import           System.Process.Typed
import           Data.Map.Strict            (toList)

import           Dampf.Postgres.Connect
import           Dampf.Postgres.Migrate
import           Dampf.Postgres.Setup
import           Dampf.Types
import           System.Environment (getEnvironment)


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
    ms <- view (config . postgres)
    ds <- view (app . databases)

    case ms of
        Just s  -> iforM_ ds $ \name spec ->
            when (maybe True (== name) mdb) $ do
                let fileName = "backup_" ++ T.unpack name ++ ".sqlc"
                let passwd   = lookupPassword (spec ^. user) s
                let envs     = [("PGDATABASE", T.unpack name)
                                , ("PGUSER", T.unpack $ spec ^. user)
                                , ("PGPASSWORD", passwd)
                                ]

                let cmd  = setEnv envs $ shell $ "pg_dump -Fc >" ++ fileName

                runProcess_ cmd

        Nothing -> throwM NoDatabaseServer

pgEnv :: Text -> DatabaseSpec -> PostgresConfig -> [(String,String)]
pgEnv dbNm d s =
    [ ("PGHOST", T.unpack $ s ^. host)
    , ("PGPORT", T.unpack $ s ^. port . to (T.pack . show))
    , ("PGDATABASE", T.unpack $ dbNm)
    , ("PGUSER", T.unpack $ d ^. user)
    , ("PGPASSWORD", T.unpack $ s ^. users . at (d ^. user) . non "")
    ]

envCmd :: (MonadIO m, MonadThrow m) =>  [Text] -> DampfT m ()
envCmd cmd = do
  oldEnv <- liftIO $ getEnvironment
  dbs <- view (app . databases)
  Just s <- view (config . postgres)
  let (dbNm, dbSpec):_ = toList dbs
      envs = pgEnv dbNm dbSpec s ++ oldEnv
      shcmd  = setEnv envs $ shell $ T.unpack $ T.unwords cmd
  runProcess_ shcmd



--PGPASSWORD=mypassword pg_dump -Fc -U myuser filocore >../db_dump

