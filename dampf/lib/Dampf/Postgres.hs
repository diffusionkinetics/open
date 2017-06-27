module Dampf.Postgres where

import Dampf.Postgres.Migrate
import Dampf.Postgres.Connect
import Dampf.AppFile
import Dampf.ConfigFile
import Dampf.Postgres.Setup
import Control.Monad (forM_, when)
import System.Process.Typed


runMigrations :: Maybe FilePath -> Maybe String -> IO ()
runMigrations mfp mdbnm = do
  withConfigFile Nothing $ \cfg -> do
    withAppFile mfp $ \(Dampfs dampfs) -> do
      forM_ [(dbnm, dbspec) | PostgresDB dbnm dbspec <- dampfs]
            $ \(dbnm, dbspec) -> do
        when (maybe True (==dbnm) mdbnm) $
          migrate cfg dbnm dbspec

newMigrationCmd :: Maybe FilePath -> Maybe String -> String -> IO ()
newMigrationCmd mfp mdbnm mignm = do
  withAppFile mfp $ \(Dampfs dampfs) -> do
    let dbs = [( dbnm, dbspec) | PostgresDB dbnm dbspec <- dampfs]
    case (dbs, mdbnm) of
      ([db], Nothing) -> newMigration mignm $ snd db
      (_, Just dbnm) -> case lookup dbnm dbs of
                          Just dbspec -> newMigration mignm dbspec
                          Nothing -> fail "cannot find database in appfile"
      _ -> fail $ "newmigration: database not specified"

setupDB :: Maybe FilePath -> IO ()
setupDB mfp = do
  withAppFile mfp $ \dampfs -> do
    withConfigFile Nothing $ \cfg -> do
      createUsers dampfs cfg
      createDatabases dampfs cfg
      createExtensions dampfs cfg

backupDB :: Maybe FilePath -> Maybe String -> IO ()
backupDB mfp mdbnm = do
  withAppFile mfp $ \(Dampfs dampfs) -> do
    withConfigFile Nothing $ \cfg -> do
      let dbs = [( dbnm, dbspec) |
                       PostgresDB dbnm dbspec <- dampfs
                       , maybe True (==dbnm) mdbnm]
      forM_ dbs $ \( dbnm, dbspec) -> do
        let outfnm = "backup_"++dbnm++".sqlc"
            passwd = lookupPassword (db_user dbspec) cfg
            envs = [("PGPASSWORD", passwd)
                   ,("PGDATABASE",dbnm )
                   ,("PGUSER", db_user dbspec) ]
            cmd = setEnv envs $ shell $ "pg_dump -Fc  >"++outfnm
        runProcess_ cmd

--PGPASSWORD=mypassword pg_dump -Fc -U myuser filocore >../db_dump