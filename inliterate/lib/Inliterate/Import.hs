{-# LANGUAGE OverloadedStrings, DefaultSignatures, TypeSynonymInstances, FlexibleInstances #-}

module Inliterate.Import where

import Data.Time
import qualified Data.Text as T
import Graphics.Plotly
import Graphics.Plotly.Lucid ()
import qualified Data.Text.Lazy.IO as TL
import Lucid
import Lucid.Bootstrap3
import Lucid.PreEscaped
import Control.Monad (unless)
import System.Environment


class AskInliterate a where
  askInliterate :: String -> a -> IO ()
  default askInliterate :: Show a => String -> a -> IO ()
  askInliterate = answerWith show

answerWith :: (a -> String) -> String -> a -> IO ()
answerWith f t x = do
  putStrLn "<pre class=\"haskell\"><code>"
  putStrLn $ concat [t, " => \n", f x]
  putStrLn "</code></pre>"

instance AskInliterate Int

instance AskInliterate Double
instance AskInliterate Float

instance AskInliterate UTCTime

instance AskInliterate String where
  askInliterate = answerWith id

instance AskInliterate T.Text where
  askInliterate = answerWith T.unpack

instance (Show a, Show b) => AskInliterate (a,b)

instance AskInliterate (Html a) where
  askInliterate _ html
     = TL.putStrLn $ renderText html

instance AskInliterate Plotly where
  askInliterate _ plt
     = TL.putStrLn $ renderText $ toHtml plt

wrapMain :: String -> IO () -> IO ()
wrapMain hdrTxt go = do
  args <- getArgs
  unless ("--no-inlit-wrap" `elem` args) $ do
    TL.putStrLn "<!DOCTYPE HTML><html>"
    TL.putStrLn $ renderText $ head_ $ do
      meta_ [charset_ "utf-8"]
      cdnCSS
      cdnThemeCSS
      cdnJqueryJS
      cdnBootstrapJS
      preEscaped $ T.pack hdrTxt
    TL.putStrLn "<body><div class=\"container\"><div class=\"row\"><div class=\"col-sm-12\">"
  go
  unless ("--no-inlit-wrap" `elem` args) $ do
    TL.putStrLn "</div></div></div></body></html>"
