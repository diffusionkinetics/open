module Dampf.Postgres where

import Dampf.Postgres.Migrate
import Dampf.AppFile
import Control.Monad (forM_, when)

runMigrations :: Maybe FilePath -> Maybe String -> IO ()
runMigrations mfp mdbnm = do
  withAppFile mfp $ \(Dampfs dampfs) -> do
    forM_ [(dbnm, dbspec) | PostgresDB dbnm dbspec <- dampfs]
           $ \(dbnm, dbspec) -> do
      when (maybe True (==dbnm) mdbnm) $
        migrate dbnm dbspec

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
