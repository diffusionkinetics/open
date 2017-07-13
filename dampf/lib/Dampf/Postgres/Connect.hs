{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Dampf.Postgres.Connect
  ( createConn
  , destroyConn
  , createSuperUserConn
  , lookupPassword
  ) where

import Control.Exception
import Control.Lens
import Database.PostgreSQL.Simple
import GHC.Conc

import Dampf.AppFile
import Dampf.ConfigFile


lookupPassword :: (HasDampfConfig c) => String -> c -> String
lookupPassword nm cfg = case cfg ^. postgres ^. users ^. at nm of
    Nothing -> error $ "no password for user "++nm++" in .dampf.cfg"
    Just pw -> pw


createConn :: (HasDampfConfig c) => String -> DBSpec -> c -> IO Connection
createConn dbnm dbspec cfg = do
   catch (createConn' dbnm dbspec cfg)
         (\(_::SomeException) -> do putStrLn "Failed to connecto to database, retrying in 10s.."
                                    threadDelay $ 10 * 1000 * 1000
                                    createConn' dbnm dbspec cfg)


createSuperUserConn :: (HasDampfConfig c) => c -> String -> IO Connection
createSuperUserConn cfg dbnm = do
   let dbspec = DBSpec { db_user = "postgres",
                         migrations = Nothing,
                         db_extensions = []
                       }

   catch (createConn' dbnm dbspec cfg)
         (\(_::SomeException) -> do putStrLn "Failed to connecto to database, retrying in 10s.."
                                    threadDelay $ 10 * 1000 * 1000
                                    createConn' dbnm dbspec cfg)


createConn' :: (HasDampfConfig c) => String -> DBSpec -> c -> IO Connection
createConn' dbnm dbspec cfg = do
  let userNm = db_user dbspec
  connect ConnectInfo
    { connectHost     = "localhost"
    , connectUser     = userNm
    , connectPassword = lookupPassword (db_user dbspec) cfg
    , connectDatabase = dbnm
    , connectPort     = 5432
    }


destroyConn :: Connection -> IO ()
destroyConn = close

