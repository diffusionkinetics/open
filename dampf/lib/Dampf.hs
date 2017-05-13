{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Dampf where

import Data.Yaml

import Dampf.AppFile

dumpYaml :: FilePath -> IO ()
dumpYaml fp = do
  Just v <- decodeFile fp
  print (v::Value)

dumpCfg :: FilePath -> IO ()
dumpCfg fp = do
  ev <- decodeFileEither fp
  case ev of
    Right v -> print (v::Dampfs)
    Left e -> fail $ show e
