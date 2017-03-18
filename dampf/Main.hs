module Main where

import System.Environment

import Dampf

main = do
  fp:_ <- getArgs
  dumpYaml fp
