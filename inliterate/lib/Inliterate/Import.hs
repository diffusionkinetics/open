{-# LANGUAGE OverloadedStrings, DefaultSignatures, TypeSynonymInstances, FlexibleInstances #-}

module Inliterate.Import where

import Data.Time
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Lucid
import Lucid.Bootstrap3
import Lucid.PreEscaped
import Control.Monad (unless)
import System.Environment
import Data.List (intercalate)

import Graphics.Plotly
import Graphics.Plotly.Lucid ()

import qualified Graphics.Svg as Svg

data CodeType = Top | Eval | Do | Hide | Fake | Twocol | Noq deriving (Show, Eq, Read, Ord)

class AskInliterate a where
  askInliterate :: String -> [CodeType] -> a -> IO ()
  default askInliterate :: Show a => String -> [CodeType] -> a -> IO ()
  askInliterate = answerWith show

answerWith :: (a -> String) -> String -> [CodeType] -> a -> IO ()
answerWith f t cts x = do
  putStr "<pre class=\"haskell\"><code>"
  putStrLn $ concat [t, " => \n", f x]
  putStrLn "</code></pre>"

instance AskInliterate Int

instance AskInliterate Double
instance AskInliterate Float

instance AskInliterate UTCTime

instance {-# OVERLAPPING #-} AskInliterate String where
  askInliterate = answerWith id

instance AskInliterate T.Text where
  askInliterate = answerWith T.unpack

instance (Show a, Show b) => AskInliterate (a,b)

instance AskInliterate (Html ()) where
  askInliterate q cts html
     | Twocol `elem` cts = do
         putStrLn "<div class=\"row\">"
         putStrLn "<div class=\"col-md-6\">"
         putStr "<pre class=\"haskell\"><code>"
         putStrLn $ q
         putStrLn "</code></pre>"
         putStrLn "</div>"
         putStrLn "<div class=\"col-md-6\">"
         TL.putStrLn $ renderText html
         putStrLn "</div>"
         putStrLn "</div>"
     | otherwise = TL.putStrLn $ renderText html


instance AskInliterate Plotly where
  askInliterate q cts plt = askInliterate q cts $ (toHtml plt :: Html ())


instance AskInliterate Svg.Element where
  askInliterate q cts svg
     | Twocol `elem` cts = do
         putStrLn "<div class=\"row\">"
         putStrLn "<div class=\"col-md-6\">"
         putStr "<pre class=\"haskell\"><code>"
         putStrLn $ q
         putStrLn "</code></pre>"
         putStrLn "</div>"
         putStrLn "<div class=\"col-md-6\">"
         TL.putStrLn $ Svg.renderText svg
         putStrLn "</div>"
         putStrLn "</div>"
     | otherwise = TL.putStrLn $ Svg.renderText svg

instance {-# OVERLAPPABLE #-} (Show a, AskInliterate a) => AskInliterate [a] where
  askInliterate = answerWith lshow where
    lshow xs = let (first5, rest) = splitAt 5 xs
                   sfirst5 = map show first5
                   (first15, rest15 ) = splitAt 15 xs
                   avgLen = realToFrac (sum $ map length sfirst5) / realToFrac (length (first5))
                   withMore ws ys = if not $ null ws then ys++["..."] else ys
               in if avgLen > (8.0::Double)
                     then "[ " ++ intercalate "\n, " (withMore rest sfirst5) ++ "]"
                     else "[" ++ intercalate "," (withMore rest15 (map show first15)) ++ "]"

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
