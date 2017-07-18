module Dampf.Postgres where

import Control.Lens
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Process.Typed

import Dampf.AppFile
import Dampf.ConfigFile
import Dampf.Postgres.Connect
import Dampf.Postgres.Migrate
import Dampf.Postgres.Setup


runMigrations :: Maybe FilePath -> Maybe String -> IO ()
runMigrations mfp mdbnm = withConfigFile Nothing $ \cfg ->
    withAppFile mfp $ \a ->
        iforM_ (a ^. databases) $ \dbnm dbspec ->
            when (maybe True (== T.unpack dbnm) mdbnm)
                $ migrate (T.unpack dbnm) dbspec cfg


newMigrationCmd :: Maybe FilePath -> Maybe String -> String -> IO ()
newMigrationCmd mfp mdbnm mignm =
  withAppFile mfp $ \a -> do
    let dbs = a ^. databases ^. to Map.toList
    case (dbs, mdbnm) of
        ([db], Nothing) -> newMigration mignm $ snd db
        (_, Just dbnm)  -> case lookup (T.pack dbnm) dbs of
            Just dbspec -> newMigration mignm dbspec
            Nothing     -> error "cannot find database in appfile"

        _ -> error "newmigration: database not specified"


setupDB :: Maybe FilePath -> IO ()
setupDB mfp = withAppFile mfp $ \dampfs ->
    withConfigFile Nothing $ \cfg -> do
      createUsers dampfs cfg
      createDatabases dampfs cfg
      createExtensions dampfs cfg


backupDB :: Maybe FilePath -> Maybe String -> IO ()
backupDB mfp mdbnm = withAppFile mfp $ \a ->
    withConfigFile Nothing $ \cfg ->
      iforM_ (a ^. databases) $ \dbnm dbspec ->
        when (maybe True (== T.unpack dbnm) mdbnm) $ do
            let outfnm = "backup_"++ T.unpack dbnm ++".sqlc"
                passwd = lookupPassword (dbspec ^. user) cfg
                envs = [("PGPASSWORD", passwd)
                       ,("PGDATABASE", T.unpack dbnm)
                       ,("PGUSER", dbspec ^. user) ]
                cmd = setEnv envs $ shell $ "pg_dump -Fc  >"++outfnm

            runProcess_ cmd


--PGPASSWORD=mypassword pg_dump -Fc -U myuser filocore >../db_dump

