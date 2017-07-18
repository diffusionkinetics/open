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


createUsers :: (HasDampfConfig c, HasDampfApp a) => a -> c -> IO ()
createUsers a cfg = do
    conn <- createSuperUserConn cfg "postgres"
    iforM_ (a ^. databases) $ \_ dbSpec -> do
        let pass = lookupPassword (dbSpec ^. user) cfg

        rls <- query conn "SELECT rolname FROM pg_roles where rolname = ?"
            (Only $ dbSpec ^. user)

        case rls :: [Only String] of
            [] -> void $ execute conn "CREATE USER ? WITH PASSWORD ?"
                (Identifier $ T.pack $ dbSpec ^. user, pass)

            _ -> return ()

    destroyConn conn


createExtensions :: (HasDampfConfig c, HasDampfApp a) => a -> c -> IO ()
createExtensions a cfg = iforM_ (a ^. databases) $ \db dbSpec -> do
    conn <- createSuperUserConn cfg (T.unpack db)
    let exts = nub $ dbSpec ^. extensions

    forM_ exts $ \ext -> void
        $ execute conn "CREATE EXTENSION IF NOT EXISTS ?"
            (Only $ Identifier $ T.pack ext)

    destroyConn conn


createDatabases :: (HasDampfConfig c, HasDampfApp a) => a -> c -> IO ()
createDatabases a cfg = do
    conn <- createSuperUserConn cfg "postgres"

    iforM_ (a ^. databases) $ \db dbSpec -> do
        dbs <- query conn "SELECT datname FROM pg_database where datname = ?"
            (Only $ T.unpack db)

        case dbs :: [Only String] of
            [] -> do
                _ <- execute conn "CREATE DATABASE ?"
                    (Only $ Identifier db)

                _ <- execute conn "GRANT ALL PRIVILEGES ON DATABASE ? to ?"
                    (Identifier db, Identifier . T.pack $ dbSpec ^. user)

                return ()

            _ -> return ()

        return ()
    destroyConn conn

