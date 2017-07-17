{-# LANGUAGE OverloadedStrings #-}

module Dampf.Postgres.Setup where

import           Control.Lens
import           Control.Monad
import           Data.List
import qualified Data.Text as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types

import           Dampf.AppFile
import           Dampf.ConfigFile
import           Dampf.Postgres.Connect


createUsers :: (HasDampfConfig c) => Dampfs -> c -> IO ()
createUsers (Dampfs dampfs) cfg = do
    conn <- createSuperUserConn cfg "postgres"
    forM_ ds $ \dbSpec -> do
        let pass = lookupPassword (dbSpec ^. dbUser) cfg

        rls <- query conn "SELECT rolname FROM pg_roles where rolname = ?"
            (Only $ dbSpec ^. dbUser)

        case rls :: [Only String] of
            [] -> void $ execute conn "CREATE USER ? WITH PASSWORD ?"
                (Identifier $ T.pack $ dbSpec ^. dbUser, pass)

            _ -> return ()

    destroyConn conn
  where
    ds = [spec | PostgresDB _ spec <- dampfs]


createExtensions :: (HasDampfConfig c) => Dampfs -> c -> IO ()
createExtensions (Dampfs dampfs) cfg = forM_ ds $ \(db, dbSpec) -> do
    conn <- createSuperUserConn cfg db
    let exts = nub $ dbSpec ^. dbExtensions

    forM_ exts $ \ext -> void
        $ execute conn "CREATE EXTENSION IF NOT EXISTS ?"
            (Only $ Identifier $ T.pack ext)

    destroyConn conn
  where
    ds = [(db, dbSpec) | PostgresDB db dbSpec <- dampfs]


createDatabases :: (HasDampfConfig c) => Dampfs -> c -> IO ()
createDatabases (Dampfs dampfs) cfg = do
    conn <- createSuperUserConn cfg "postgres"

    forM_ ds $ \(db, dbSpec) -> do
        dbs <- query conn "SELECT datname FROM pg_database where datname = ?"
            (Only db)

        case dbs :: [Only String] of
            [] -> do
                _ <- execute conn "CREATE DATABASE ?"
                    (Only . Identifier $ T.pack db)

                _ <- execute conn "GRANT ALL PRIVILEGES ON DATABASE ? to ?"
                    (Identifier $ T.pack db, Identifier . T.pack $ dbSpec ^. dbUser)

                return ()

            _ -> return ()

        return ()
    destroyConn conn
  where
    ds = [(db, dbSpec) | PostgresDB db dbSpec <- dampfs]

