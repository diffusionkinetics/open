{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Dampf where

import Data.Yaml

import Dampf.AppFile
import Dampf.Docker

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

goBuild :: Maybe FilePath -> IO ()
goBuild mfp = do
  withAppFile mfp $ \dampfs -> do
    buildDocker dampfs


goDeploy :: Maybe FilePath -> IO ()
goDeploy mfp = do
  withAppFile mfp $ \dampfs -> do
    deployDocker dampfs
