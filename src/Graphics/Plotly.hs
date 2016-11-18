{-# LANGUAGE DeriveGeneric, OverloadedStrings,FlexibleInstances #-}

module Graphics.Plotly where

import Data.Aeson
import Data.Aeson.Types
import Lucid
import Data.Char (toLower)
import Data.List (intercalate, stripPrefix)
import Data.Monoid ((<>))
import Data.Text (pack, Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import GHC.Generics

data Mode = Markers | Lines deriving Show

instance ToJSON [Mode] where
  toJSON = toJSON . intercalate "+" . map (map toLower . show)

data TraceType = Scatter | Bar deriving Show

instance ToJSON TraceType where
  toJSON = toJSON . map toLower . show

data Color = RGBA Int Int Int Int
           | RGB Int Int Int
           | ColIxs [Int]
           | Cols [Color]

instance ToJSON Color where
  toJSON (RGB r g b) = toJSON $ "rbg("<>show r<>","<>show g<>","<>show b<>")"
  toJSON (RGBA r g b a) = toJSON $ "rbg("<>show r<>","<>show g<>","<>show b<>","<> show a<>")"
  toJSON (ColIxs cs) = toJSON cs
  toJSON (Cols cs) = toJSON cs

data Symbol = Circle | Square | Diamond | Cross deriving Show

instance ToJSON Symbol where
  toJSON = toJSON . map toLower . show


data Marker = Marker
  { size :: Maybe Int
  , markercolor :: Maybe Color
  , symbol :: Maybe Symbol
  , opacity :: Maybe Double
  } deriving Generic

instance ToJSON Marker where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True }

data Dash = Solid | Dashdot | Dot deriving Show

instance ToJSON Dash where
  toJSON = toJSON . map toLower . show

instance ToJSON Barmode where
  toJSON = toJSON . map toLower . show

data Barmode = Stack | Group deriving Show

data Orientation = Horizontal | Vertical

instance ToJSON Orientation where
  toJSON Horizontal = "h"
  toJSON Vertical = "v"

data Line = Line
  { linewidth :: Maybe Double
  , linecolor :: Maybe Color
  , dash :: Maybe Dash
  } deriving Generic

instance ToJSON Line where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True,
                                         fieldLabelModifier = dropInitial "line"}

defLine :: Line
defLine = Line Nothing Nothing Nothing

defMarker :: Marker
defMarker  = Marker Nothing Nothing Nothing Nothing

data Trace = Trace
  { x :: Maybe [Double]
  , y :: Maybe [Double]
  , xtext :: Maybe [Text]
  , ytext :: Maybe [Text]
  , mode :: Maybe [Mode]
  , name :: Maybe Text
  , text :: Maybe [Text]
  , tracetype :: TraceType
  , marker :: Maybe Marker
  , line :: Maybe Line
  } deriving Generic

scatter :: Trace
scatter = Trace Nothing Nothing Nothing Nothing Nothing Nothing Nothing Scatter Nothing Nothing

instance ToJSON Trace where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True,
                                         fieldLabelModifier = rename "xtext" "x" . rename "ytext" "y" . dropLeadingUnderscore}


dropLeadingUnderscore :: String -> String
dropLeadingUnderscore ('_':s) = s
dropLeadingUnderscore s = s

dropInitial :: String -> String -> String
dropInitial s s' = case stripPrefix s s' of
                  Nothing -> s'
                  Just s'' -> s''

data Axis = Axis
  { range :: Maybe (Double,Double)
  , axistitle :: Maybe Text
  , showgrid :: Bool
  , zeroline :: Bool
  } deriving Generic

instance ToJSON Axis where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True,
                                         fieldLabelModifier = rename "axistitle" "axis"}

defAxis :: Axis
defAxis = Axis Nothing Nothing True False

rename :: String -> String -> String -> String
rename froms tos s | s == froms = tos
                   | otherwise = s


data Layout = Layout
  { xaxis :: Maybe (Double,Double)
  , yaxis :: Maybe (Double,Double)
  , title :: Maybe Text
  , showlegend :: Bool
  , height :: Maybe Int
  , width :: Maybe Int
  , barmode :: Maybe Barmode
  } deriving Generic

defLayout :: Layout
defLayout = Layout Nothing Nothing Nothing False Nothing Nothing Nothing

instance ToJSON Layout where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

newPlot :: String -> [Trace] -> Layout -> Html ()
newPlot divNm trs lay =
  let trJSON = decodeUtf8 $ toStrict $ encode trs
      layoutJSON = {-case mlay of
                     Nothing -> ""
                     Just lay -> -} ","<>(decodeUtf8 $ toStrict $ encode lay)
  in script_ ("Plotly.newPlot('"<>pack divNm<>"', "<>trJSON<>layoutJSON<>");")
