{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Dampf.Postgres.Connect
  ( createConn
  , destroyConn
  ) where

import           Control.Exception

import           GHC.Conc
import Dampf.AppFile

import Database.PostgreSQL.Simple

createConn :: String -> DBSpec -> IO Connection
createConn dbnm config = do
   catch (createConn' dbnm config)
         (\(_::SomeException) -> do putStrLn "Failed to connecto to database, retrying in 10s.."
                                    threadDelay $ 10 * 1000 * 1000
                                    createConn' dbnm config)


createConn' :: String -> DBSpec -> IO Connection
createConn' dbnm config = do
  connect ConnectInfo
    { connectHost     = "localhost"
    , connectUser     = db_user config
    , connectPassword = db_password config
    , connectDatabase = dbnm
    , connectPort     = 5432
    }

destroyConn :: Connection -> IO ()
destroyConn = close