{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Generic
import Data.Maybe


import Dampf
import Dampf.Postgres

data Cmd = DumpYaml {appFile :: Maybe FilePath}
         | Dump {appFile :: Maybe FilePath}
         | RunMigrations { appFile :: Maybe FilePath
                         , onlyDatabase :: Maybe String }
         | NewMigration { appFile :: Maybe FilePath
                        , database :: Maybe String
                        , name :: String }
         | Build {appFile :: Maybe FilePath}
         | Deploy {appFile :: Maybe FilePath}
         | SetupDB {appFile :: Maybe FilePath}
  deriving (Generic, Show)


instance ParseRecord Cmd

main :: IO ()
main = do
    x <- getRecord "Test program"
    dispatch x

dispatch :: Cmd -> IO ()
dispatch (DumpYaml mfp) = dumpYaml (fromMaybe "dampf.yaml" mfp)
dispatch (Dump mfp) = dumpCfg (fromMaybe "dampf.yaml" mfp)
dispatch (Dump mfp) = dumpCfg (fromMaybe "dampf.yaml" mfp)
dispatch (RunMigrations mfp mdbnm) = runMigrations mfp mdbnm
dispatch (NewMigration mfp mdbnm mignm) = newMigrationCmd mfp mdbnm mignm
dispatch (Build mfp) = goBuild mfp
dispatch (Deploy mfp) = goDeploy mfp
dispatch (SetupDB mfp) = setupDB mfp

