{-# LANGUAGE OverloadedStrings #-}

module Dampf where

import Dampf.AppFile
import Dampf.ConfigFile
import Dampf.Docker
import Dampf.Postgres
import Dampf.Postgres.Setup
import Dampf.Nginx


dumpApp :: FilePath -> IO ()
dumpApp f = loadAppFile (Just f) >>= print


dumpConfig :: FilePath -> IO ()
dumpConfig f = loadConfigFile (Just f) >>= print


goBuild :: Maybe FilePath -> IO ()
goBuild mfp = do
  setupDB mfp

  withAppFile mfp $ \dampfs ->
    withConfigFile Nothing $ \cfg -> do
      buildDocker dampfs
      createUsers dampfs cfg
      createDatabases dampfs cfg
      createExtensions dampfs cfg



goDeploy :: Maybe FilePath -> IO ()
goDeploy mfp = do
  goBuild mfp

  withAppFile mfp $ \dampfs ->
    withConfigFile Nothing $ \cfg -> do
      deployDocker dampfs
      runMigrations mfp Nothing
      deployDomains cfg dampfs
