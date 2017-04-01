{-# LANGUAGE OverloadedStrings, DefaultSignatures #-}

module Inliterate.Class where

import qualified Data.Text as T
import qualified Data.Text.IO as T

class AskInliterate a where
  askInliterate :: T.Text -> a -> IO ()
  default askInliterate :: Show a => T.Text -> a -> IO ()
  askInliterate t x = T.putStrLn $ T.concat [t, " => ", T.pack $ show x]

instance AskInliterate Int
