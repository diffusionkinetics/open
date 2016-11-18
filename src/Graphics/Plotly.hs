{-# LANGUAGE DeriveGeneric, OverloadedStrings,FlexibleInstances #-}

module Graphics.Plotly where

import Data.Aeson
import Data.Aeson.Types
import Lucid
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import GHC.Generics

data Mode = Markers | Lines deriving Show

instance ToJSON [Mode] where
  toJSON = toJSON . intercalate "+" . map (map toLower . show)

data Type_ = Scatter deriving Show

instance ToJSON Type_ where
  toJSON = toJSON . map toLower . show


data Trace = Trace
  { x :: [Double]
  , y :: [Double]
  , mode :: [Mode]
  , _type :: Type_
  } deriving Generic

instance ToJSON Trace where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True,
                                         fieldLabelModifier = dropLeadingUnderscore}


dropLeadingUnderscore :: String -> String
dropLeadingUnderscore ('_':s) = s
dropLeadingUnderscore s = s

data Layout = Layout deriving Generic

instance ToJSON Layout where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

newPlot :: String -> Trace -> Maybe Layout -> Html ()
newPlot divNm _tr _ =
  let trJSON = decodeUtf8 $ toStrict $ encode _tr
  in script_ ("Plotly.newPlot('"<>pack divNm<>"', ["<>trJSON<>"]);")
