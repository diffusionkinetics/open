{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Dampf.Postgres.Connect
  ( createConn
  , destroyConn
  , createSuperUserConn
  ) where

import           Control.Exception

import           GHC.Conc
import Dampf.AppFile
import Dampf.ConfigFile

import Database.PostgreSQL.Simple

createConn :: String -> DBSpec -> IO Connection
createConn dbnm dbspec = do
   catch (createConn' dbnm dbspec)
         (\(_::SomeException) -> do putStrLn "Failed to connecto to database, retrying in 10s.."
                                    threadDelay $ 10 * 1000 * 1000
                                    createConn' dbnm dbspec)

createSuperUserConn :: DampfConfig -> String -> IO Connection
createSuperUserConn cfg dbnm = do
   let dbspec = DBSpec { db_user = "postgres",
                         db_password = postgres_password cfg,
                         migrations = Nothing,
                         db_extensions = []
                       }

   catch (createConn' dbnm dbspec)
         (\(_::SomeException) -> do putStrLn "Failed to connecto to database, retrying in 10s.."
                                    threadDelay $ 10 * 1000 * 1000
                                    createConn' dbnm dbspec)


createConn' :: String -> DBSpec -> IO Connection
createConn' dbnm dbspec = do
  connect ConnectInfo
    { connectHost     = "localhost"
    , connectUser     = db_user dbspec
    , connectPassword = db_password dbspec
    , connectDatabase = dbnm
    , connectPort     = 5432
    }

destroyConn :: Connection -> IO ()
destroyConn = close