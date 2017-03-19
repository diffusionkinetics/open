module Dampf where

import Data.Yaml



dumpYaml :: FilePath -> IO ()
dumpYaml fp = do
  Just v <- decodeFile fp
  print (v::Value)
