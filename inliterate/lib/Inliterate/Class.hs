{-# LANGUAGE OverloadedStrings, DefaultSignatures, TypeSynonymInstances, FlexibleInstances #-}

module Inliterate.Class where

import Data.Time
import Graphics.Plotly
import Graphics.Plotly.Lucid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Lucid

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

instance AskInliterate (Html a) where
  askInliterate _ html
     = TL.putStrLn $ renderText html

instance AskInliterate Plotly where
  askInliterate _ plt
     = TL.putStrLn $ renderText $ toHtml plt
