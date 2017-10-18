module Dampf.Docker.Args.Class where

import           Data.Text      (Text)
import qualified Data.Text as T


class ToArgs a where
    toArgs :: a -> [String]


-- Utility Functions

flagArg :: (Show a) => a -> [String]
flagArg v = if null s then [] else [s]
  where
    s = show v


namedArg :: (Show a) => String -> a -> [String]
namedArg n v = ["--" ++ n ++ "=" ++ show v]


namedTextArg :: String -> Text -> [String]
namedTextArg n v = ["--" ++ n ++ "=" ++ T.unpack v]

