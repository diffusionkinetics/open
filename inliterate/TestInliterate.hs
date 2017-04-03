module Main where

import Inliterate
import System.Environment
import qualified Data.Text.IO as T

main = do
  putStr "\n"
  dumpDoc "example.ihs"
  d <- readDoc "example.ihs"

  T.putStrLn $ genHaskell d
