{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text        (Text)
import Options.Generic

import Dampf
import Dampf.AppFile
import Dampf.ConfigFile
import Dampf.Postgres
import Dampf.Types


data Cmd
    = Backup
        { onlyDatabase  :: Maybe Text
        }
    | Build
    | Deploy
    | Dump
    | NewMigration
        { database      :: Text
        , name          :: FilePath
        }
    | RunMigrations
        { onlyDatabase  :: Maybe Text
        }
    | SetupDB
    deriving (Show, Generic)


instance ParseRecord Cmd


main :: IO ()
main = do
    cmd <- getRecord "Test program"
    a   <- loadAppFile Nothing
    c   <- loadConfigFile Nothing
    
    case cmd of
        Backup db           -> runDampfT a c (backupDB db)
        Build               -> runDampfT a c goBuild
        Deploy              -> runDampfT a c goDeploy
        Dump                -> runDampfT a c dump
        NewMigration db mig -> runDampfT a c (newMigrationCmd db mig)
        RunMigrations db    -> runDampfT a c (runMigrations db)
        SetupDB             -> runDampfT a c setupDB

