{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Generic
import Data.Maybe


import Dampf

data Cmd = DumpYaml (Maybe FilePath) 
         | Dump (Maybe FilePath) 
  deriving (Generic, Show)


instance ParseRecord Cmd

main :: IO ()
main = do
    x <- getRecord "Test program"
    dispatch x

dispatch :: Cmd -> IO ()
dispatch (DumpYaml mfp) = dumpYaml (fromMaybe "dampf.yaml" mfp)
dispatch (Dump mfp) = dumpCfg (fromMaybe "dampf.yaml" mfp)
