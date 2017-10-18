module Main where

import Inliterate
import Inliterate.Inspect
import System.Environment
import qualified Data.Text.IO as T

main = do
  putStr "\n"
  dumpDoc "TestInliteratePreProc.hs"
  d <- readDoc "TestInliteratePreProc.hs"

  T.putStrLn $ genHaskell d
