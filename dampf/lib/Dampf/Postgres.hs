module Dampf.Postgres where

import Control.Lens
import Control.Monad (forM_, when)
import System.Process.Typed

import Dampf.AppFile
import Dampf.ConfigFile
import Dampf.Postgres.Connect
import Dampf.Postgres.Migrate
import Dampf.Postgres.Setup


runMigrations :: Maybe FilePath -> Maybe String -> IO ()
runMigrations mfp mdbnm = withConfigFile Nothing $ \cfg ->
    withAppFile mfp $ \(Dampfs dampfs) ->
        forM_ [(dbnm, dbspec) | PostgresDB dbnm dbspec <- dampfs] $ \(dbnm, dbspec) ->
            when (maybe True (==dbnm) mdbnm) $ migrate dbnm dbspec cfg


newMigrationCmd :: Maybe FilePath -> Maybe String -> String -> IO ()
newMigrationCmd mfp mdbnm mignm =
  withAppFile mfp $ \(Dampfs dampfs) -> do
    let dbs = [( dbnm, dbspec) | PostgresDB dbnm dbspec <- dampfs]
    case (dbs, mdbnm) of
        ([db], Nothing) -> newMigration mignm $ snd db
        (_, Just dbnm)  -> case lookup dbnm dbs of
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
backupDB mfp mdbnm = withAppFile mfp $ \(Dampfs dampfs) ->
    withConfigFile Nothing $ \cfg -> do
      let dbs = [(dbnm, dbspec) | PostgresDB dbnm dbspec <- dampfs, maybe True (==dbnm) mdbnm]
      forM_ dbs $ \( dbnm, dbspec) -> do
        let outfnm = "backup_"++dbnm++".sqlc"
            passwd = lookupPassword (dbspec ^. dbUser) cfg
            envs = [("PGPASSWORD", passwd)
                   ,("PGDATABASE",dbnm )
                   ,("PGUSER", dbspec ^. dbUser) ]
            cmd = setEnv envs $ shell $ "pg_dump -Fc  >"++outfnm

        runProcess_ cmd


--PGPASSWORD=mypassword pg_dump -Fc -U myuser filocore >../db_dump

