{-|

Helper functions for defining valid JSON instances

-}

module Graphics.Plotly.Utils where

import Data.List (stripPrefix)
import Data.Aeson.Types

unLens :: String -> String
unLens ('_':s) = s
unLens s = s

dropInitial :: String -> String -> String
dropInitial s s' = case stripPrefix s s' of
                  Nothing -> s'
                  Just s'' -> s''

rename :: String -> String -> String -> String
rename froms tos s | s == froms = tos
                   | otherwise = s

jsonOptions :: Options
jsonOptions = defaultOptions {omitNothingFields = True,
                              fieldLabelModifier = unLens }
