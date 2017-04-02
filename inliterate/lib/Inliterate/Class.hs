{-# LANGUAGE OverloadedStrings, DefaultSignatures #-}

module Inliterate.Class where

import Data.Time

class AskInliterate a where
  askInliterate :: String -> a -> IO ()
  default askInliterate :: Show a => String -> a -> IO ()
  askInliterate = answerWith show

answerWith :: (a -> String) -> String -> a -> IO ()
answerWith f t x = putStrLn $ concat [t, " => ", f x]

instance AskInliterate Int

instance AskInliterate Double
instance AskInliterate Float

instance AskInliterate UTCTime

instance (Show a, Show b) => AskInliterate (a,b)
