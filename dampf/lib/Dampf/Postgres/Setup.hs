{-# LANGUAGE OverloadedStrings #-}

module Dampf.Postgres.Setup where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.List
import qualified Data.Text as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types

import           Dampf.Postgres.Connect
import           Dampf.Types


createUsers :: (MonadIO m, MonadThrow m) => DampfT m ()
createUsers = do
    ms <- view (config . postgres)
    ds <- view (app . databases)

    case ms of
        Just s  -> iforM_ ds $ \name spec -> do
            conn <- createSuperUserConn name
            let pass = lookupPassword (spec ^. user) s

            liftIO $ do
                rls <- query conn "SELECT rolname FROM pg_roles WHERE rolname = ?"
                    (Only $ spec ^. user)

                case rls :: [Only String] of
                    [] -> void $ execute conn "CREATE USER ? WITH PASSWORD ?"
                        (Identifier $ T.pack $ spec ^. user, pass)

                    _  -> return ()

            destroyConn conn

        Nothing -> throwM NoDatabaseServer

            
createExtensions :: (MonadIO m, MonadThrow m) => DampfT m ()
createExtensions = do
    ms <- view (config . postgres)
    ds <- view (app . databases)

    case ms of
        Just _  -> iforM_ ds $ \name spec -> do
            conn <- createSuperUserConn name
            let exts = nub $ spec ^. extensions

            liftIO . forM_ exts $ \ext -> void
                $ execute conn "CREATE EXTENSION IF NOT EXISTS ?"
                    (Only $ Identifier $ T.pack ext)

            destroyConn conn

        Nothing -> throwM NoDatabaseServer


createDatabases :: (MonadIO m, MonadThrow m) => DampfT m ()
createDatabases = do
    ms <- view (config . postgres)
    ds <- view (app . databases)

    case ms of
        Just _  -> iforM_ ds $ \name spec -> do
            conn <- createSuperUserConn name

            liftIO $ do
                dbs  <- query conn
                    "SELECT datname FROM pg_database WHERE datname = ?"
                    (Only $ T.unpack name)

                case dbs :: [Only String] of
                    [] -> do
                        void $ execute conn "CREATE DATABASE ?"
                            (Only $ Identifier name)

                        void $ execute conn
                            "GRANT ALL PRIVILEGES ON DATABASE ? TO ?"
                            (Identifier name, Identifier . T.pack $ spec ^. user)

                    _  -> return ()

            destroyConn conn

        Nothing -> throwM NoDatabaseServer

