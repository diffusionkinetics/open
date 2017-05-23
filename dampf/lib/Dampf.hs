{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Dampf where

import Data.Yaml

import Dampf.AppFile
import Dampf.ConfigFile
import Dampf.Docker
import Dampf.Postgres
import Dampf.Postgres.Setup
import Dampf.Nginx

dumpYaml :: FilePath -> IO ()
dumpYaml fp = do
  Just v <- decodeFile fp
  print (v::Value)

dumpCfg :: FilePath -> IO ()
dumpCfg fp = do
  ev <- decodeFileEither fp
  case ev of
    Right (Dampfs v) -> mapM_ print v
    Left e -> fail $ show e
  withConfigFile Nothing $ \cfg -> do
    print cfg

goBuild :: Maybe FilePath -> IO ()
goBuild mfp = do
  setupDB mfp
  withAppFile mfp $ \dampfs -> do
    withConfigFile Nothing $ \cfg -> do
      buildDocker dampfs
      createUsers dampfs cfg
      createDatabases dampfs cfg
      createExtensions dampfs cfg



goDeploy :: Maybe FilePath -> IO ()
goDeploy mfp = do
  goBuild mfp
  withAppFile mfp $ \dampfs -> do
    withConfigFile Nothing $ \_ -> do
      deployDocker dampfs
      runMigrations mfp Nothing
      deployDomains dampfs