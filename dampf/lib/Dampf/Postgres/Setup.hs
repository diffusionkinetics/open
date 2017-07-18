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
createUsers a c = iforM_ (c ^. databaseServers) $ \server cfg ->
    iforM_ (a ^. databases) $ \name spec -> do
        conn <- createSuperUserConn (T.unpack server) (T.unpack name) c
        let pass = lookupPassword (spec ^. user) cfg

        rls <- query conn "SELECT rolname FROM pg_roles where rolname = ?"
            (Only $ spec ^. user)

        case rls :: [Only String] of
            [] -> void $ execute conn "CREATE USER ? WITH PASSWORD ?"
                (Identifier $ T.pack $ spec ^. user, pass)

            _ -> return ()

        destroyConn conn


createExtensions :: (HasDampfConfig c, HasDampfApp a) => a -> c -> IO ()
createExtensions a c = iforM_ (c ^. databaseServers) $ \server _ ->
    iforM_ (a ^. databases) $ \name spec -> do
        conn <- createSuperUserConn (T.unpack server) (T.unpack name) c
        let exts = nub $ spec ^. extensions

        forM_ exts $ \ext -> void
            $ execute conn "CREATE EXTENSION IF NOT EXISTS ?"
                (Only $ Identifier $ T.pack ext)

        destroyConn conn


createDatabases :: (HasDampfConfig c, HasDampfApp a) => a -> c -> IO ()
createDatabases a c = iforM_ (c ^. databaseServers) $ \server _ ->
    iforM_ (a ^. databases) $ \name spec -> do
        conn <- createSuperUserConn (T.unpack server) (T.unpack name) c

        dbs <- query conn "SELECT datname FROM pg_database where datname = ?"
            (Only $ T.unpack name)

        case dbs :: [Only String] of
            [] -> do
                _ <- execute conn "CREATE DATABASE ?"
                        (Only $ Identifier name)

                _ <- execute conn "GRANT ALL PRIVILEGES ON DATABASE ? to ?"
                        (Identifier name, Identifier . T.pack $ spec ^. user)

                return ()

            _ -> return ()

        destroyConn conn

