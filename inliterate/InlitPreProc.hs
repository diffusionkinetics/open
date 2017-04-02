module Main where

import Inliterate
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T


main = do
  _:inFile:outFile:_ <- getArgs
  d <- readDoc inFile
  T.writeFile outFile $ genHaskell d
