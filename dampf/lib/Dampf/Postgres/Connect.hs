{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Dampf.Postgres.Connect
  ( createConn
  , destroyConn
  , createSuperUserConn
  , lookupPassword
  ) where

import           Control.Exception
import           Control.Lens
import qualified Data.Text as T
import           Database.PostgreSQL.Simple
import           GHC.Conc

import           Dampf.AppFile
import           Dampf.ConfigFile


lookupPassword :: (HasPostgresConfig c) => String -> c -> String
lookupPassword name cfg = case cfg ^. users . at (T.pack name) of
    Nothing -> error $ "no password for user "++ name ++ " in .dampf.cfg"
    Just pw -> T.unpack pw


createSuperUserConn :: (HasDampfConfig c)
    => String -> String -> c -> IO Connection
createSuperUserConn server name = createConn server name spec
  where
    spec = DatabaseSpec Nothing "postgres" []


createConn :: (HasDampfConfig c)
    => String -> String -> DatabaseSpec -> c -> IO Connection
createConn server name spec cfg = catch (createConn' server name spec cfg) $
    \(_::SomeException) -> do
        putStrLn "Failed to connect to to database, retrying in 10s.."
        threadDelay $ 10 * 1000 * 1000
        createConn' server name spec cfg


createConn' :: (HasDampfConfig c)
    => String -> String -> DatabaseSpec -> c -> IO Connection
createConn' server name spec cfg = case dbCfg of
    Just c  -> connect ConnectInfo
        { connectHost     = c ^. host
        , connectUser     = spec ^. user
        , connectPassword = lookupPassword (spec ^. user) c
        , connectDatabase = name
        , connectPort     = c ^. port ^. to fromIntegral
        }

    Nothing -> error "Database does not exist"
  where
    dbCfg = cfg ^. databaseServers . at (T.pack server)


destroyConn :: Connection -> IO ()
destroyConn = close

