{-# LANGUAGE OverloadedStrings #-}

module Dampf.Postgres.Setup where

import Control.Monad

import Data.List
import Dampf.AppFile
import Dampf.ConfigFile
import Dampf.Postgres.Connect
import qualified Data.Text as T

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

createUsers :: Dampfs -> DampfConfig -> IO ()
createUsers (Dampfs dampfs) cfg = do
  conn <- createSuperUserConn cfg "postgres"
  let dbs = [ dbspec | PostgresDB _ dbspec <- dampfs ]
  forM_ dbs $ \dbspec -> do
    let passwd = lookupPassword (dbUser dbspec) cfg
    rls <- query conn "SELECT rolname FROM pg_roles where rolname = ?"
       (Only $ dbUser dbspec)
    case rls :: [Only String] of
      [] -> void $ execute conn "CREATE USER ? WITH PASSWORD ?"
                    (Identifier $ T.pack $ dbUser dbspec,passwd)
      _ -> return ()

    return ()
  destroyConn conn

createExtensions :: Dampfs -> DampfConfig -> IO ()
createExtensions (Dampfs dampfs) cfg = do
  let dbs = [( dbnm, dbspec) | PostgresDB dbnm dbspec <- dampfs ]
  forM_ dbs $ \(dbnm, dbspec) -> do
    conn <- createSuperUserConn cfg dbnm
    let exts = nub $ dbExtensions dbspec
    forM_ exts $ \ext -> do
      void $ execute conn "CREATE EXTENSION IF NOT EXISTS ?"
                    (Only $ Identifier $ T.pack ext)
    destroyConn conn

createDatabases :: Dampfs -> DampfConfig -> IO ()
createDatabases (Dampfs dampfs) cfg = do
  conn <- createSuperUserConn cfg "postgres"
  let dbs' = [( dbnm, dbspec) | PostgresDB dbnm dbspec <- dampfs ]
  forM_ dbs' $ \(dbnm, dbspec) -> do
    dbs <- query conn "SELECT datname FROM pg_database where datname = ?"
       (Only $ dbnm)
    case dbs :: [Only String] of
      [] -> do execute conn "CREATE DATABASE ?"
                    (Only $ Identifier $ T.pack dbnm)
               execute conn "GRANT ALL PRIVILEGES ON DATABASE ? to ?"
                    (Identifier $ T.pack dbnm, Identifier $ T.pack $ dbUser dbspec)
               return ()
      _ -> return ()
    return ()
  destroyConn conn

