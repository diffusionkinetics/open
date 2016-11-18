{-# LANGUAGE DeriveGeneric, OverloadedStrings,FlexibleInstances #-}

module Graphics.Plotly where

import Data.Aeson
import Data.Aeson.Types
import Lucid
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Text (pack, Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import GHC.Generics

data Mode = Markers | Lines deriving Show

instance ToJSON [Mode] where
  toJSON = toJSON . intercalate "+" . map (map toLower . show)

data Type_ = Scatter deriving Show

instance ToJSON Type_ where
  toJSON = toJSON . map toLower . show

data Color = RGBA Int Int Int Int
           | RGB Int Int Int
           | Cols [Int]

instance ToJSON Color where
  toJSON (RGB r g b) = toJSON $ "rbg("<>show r<>","<>show g<>","<>show b<>")"
  toJSON (RGBA r g b a) = toJSON $ "rbg("<>show r<>","<>show g<>","<>show b<>","<> show a<>")"
  toJSON (Cols cs) = toJSON cs

data Symbol = Circle deriving Show

instance ToJSON Symbol where
  toJSON = toJSON . map toLower . show


data Marker = Marker
  { size :: Int
  , color :: Maybe Color
  , symbol :: Symbol} deriving Generic

instance ToJSON Marker

data Trace = Trace
  { x :: [Double]
  , y :: [Double]
  , mode :: [Mode]
  , name :: Maybe Text
  , text :: Maybe [Text]
  , _type :: Type_
  } deriving Generic

scatter :: Trace
scatter = Trace [] [] [Markers] Nothing Nothing Scatter

instance ToJSON Trace where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True,
                                         fieldLabelModifier = dropLeadingUnderscore}


dropLeadingUnderscore :: String -> String
dropLeadingUnderscore ('_':s) = s
dropLeadingUnderscore s = s

data Layout = Layout
  { xaxis :: Maybe (Double,Double)
  , yaxis :: Maybe (Double,Double)
  , title :: Maybe Text
  } deriving Generic

layout :: Layout
layout = Layout Nothing Nothing Nothing

instance ToJSON Layout where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

newPlot :: String -> [Trace] -> Layout -> Html ()
newPlot divNm trs lay =
  let trJSON = decodeUtf8 $ toStrict $ encode trs
      layoutJSON = {-case mlay of
                     Nothing -> ""
                     Just lay -> -} ","<>(decodeUtf8 $ toStrict $ encode lay)
  in script_ ("Plotly.newPlot('"<>pack divNm<>"', "<>trJSON<>layoutJSON<>");")
