{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Dampf.Postgres.Connect
  ( createConn
  , destroyConn
  , createSuperUserConn
  , lookupPassword
  ) where

import           Control.Exception

import           GHC.Conc
import Dampf.AppFile
import Dampf.ConfigFile

import Database.PostgreSQL.Simple
import qualified Data.Map.Strict as Map


lookupPassword :: String -> DampfConfig -> String
lookupPassword nm cfg =
  case Map.lookup nm $ dbPasswords cfg of
    Nothing -> error $ "no password for user "++nm++" in .dampf.cfg"
    Just pw -> pw

createConn :: String -> DBSpec -> DampfConfig-> IO Connection
createConn dbnm dbspec cfg = do
   catch (createConn' dbnm dbspec cfg)
         (\(_::SomeException) -> do putStrLn "Failed to connecto to database, retrying in 10s.."
                                    threadDelay $ 10 * 1000 * 1000
                                    createConn' dbnm dbspec cfg)

createSuperUserConn :: DampfConfig -> String -> IO Connection
createSuperUserConn cfg dbnm = do
   let dbspec = DBSpec { db_user = "postgres",
                         migrations = Nothing,
                         db_extensions = []
                       }

   catch (createConn' dbnm dbspec cfg)
         (\(_::SomeException) -> do putStrLn "Failed to connecto to database, retrying in 10s.."
                                    threadDelay $ 10 * 1000 * 1000
                                    createConn' dbnm dbspec cfg)


createConn' :: String -> DBSpec -> DampfConfig-> IO Connection
createConn' dbnm dbspec cfg = do
  let userNm = db_user dbspec
  connect ConnectInfo
    { connectHost     = "localhost"
    , connectUser     = userNm
    , connectPassword = if userNm == "postgres"
                          then postgresPassword cfg
                          else lookupPassword (db_user dbspec) cfg
    , connectDatabase = dbnm
    , connectPort     = 5432
    }

destroyConn :: Connection -> IO ()
destroyConn = close
