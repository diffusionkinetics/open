{-# LANGUAGE DeriveGeneric, OverloadedStrings,FlexibleInstances, TemplateHaskell #-}

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
import Lens.Micro.TH

import Graphics.Plotly.Utils

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
  { _size :: Maybe Int
  , _markercolor :: Maybe Color
  , _symbol :: Maybe Symbol
  , _opacity :: Maybe Double
  } deriving Generic

makeLenses ''Marker

instance ToJSON Marker where
  toJSON = genericToJSON jsonOptions

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

data Fill = ToZeroY | ToNextY deriving Show

instance ToJSON Fill where
  toJSON = toJSON . map toLower . show


data Line = Line
  { _linewidth :: Maybe Double
  , _linecolor :: Maybe Color
  , _dash :: Maybe Dash
  } deriving Generic

makeLenses ''Line

instance ToJSON Line where
  toJSON = genericToJSON jsonOptions { fieldLabelModifier = dropInitial "line" . unLens}

defLine :: Line
defLine = Line Nothing Nothing Nothing

defMarker :: Marker
defMarker  = Marker Nothing Nothing Nothing Nothing

data Trace = Trace
  { _x :: Maybe [Double]
  , _y :: Maybe [Double]
  , _xtext :: Maybe [Text]
  , _ytext :: Maybe [Text]
  , _mode :: Maybe [Mode]
  , _name :: Maybe Text
  , _text :: Maybe [Text]
  , _tracetype :: TraceType
  , _marker :: Maybe Marker
  , _line :: Maybe Line
  , _fill :: Maybe Fill
  } deriving Generic


makeLenses ''Trace

scatter :: Trace
scatter = Trace Nothing Nothing Nothing Nothing Nothing Nothing Nothing Scatter Nothing Nothing Nothing

instance ToJSON Trace where
  toJSON = genericToJSON jsonOptions {fieldLabelModifier = rename "xtext" "x" . rename "ytext" "y" . unLens}


data Axis = Axis
  { _range :: Maybe (Double,Double)
  , _axistitle :: Maybe Text
  , _showgrid :: Maybe Bool
  , _zeroline :: Maybe Bool
  } deriving Generic

makeLenses ''Axis


instance ToJSON Axis where
  toJSON = genericToJSON jsonOptions {fieldLabelModifier = rename "axistitle" "axis" . unLens}

defAxis :: Axis
defAxis = Axis Nothing Nothing Nothing Nothing


data Margin = Margin
  { _marginl :: Int
  , _marginr :: Int
  , _marginb :: Int
  , _margint :: Int
  , _marginpad :: Int
  } deriving Generic

makeLenses ''Margin

instance ToJSON Margin where
  toJSON = genericToJSON jsonOptions { fieldLabelModifier = dropInitial "margin" . unLens}

thinMargins, titleMargins :: Margin
thinMargins = Margin 40 25 30 10 4
titleMargins = Margin 40 25 30 40 4

data Layout = Layout
  { _xaxis :: Maybe (Double,Double)
  , _yaxis :: Maybe (Double,Double)
  , _title :: Maybe Text
  , _showlegend :: Maybe Bool
  , _height :: Maybe Int
  , _width :: Maybe Int
  , _barmode :: Maybe Barmode
  , _margin :: Maybe Margin
  } deriving Generic

makeLenses ''Layout

defLayout :: Layout
defLayout = Layout Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON Layout where
  toJSON = genericToJSON jsonOptions

data Plotly = Plotly
  { _traces :: [Trace]
  , _layout :: Layout
  }

makeLenses ''Plotly

plotly :: [Trace] -> Plotly
plotly trs = Plotly trs defLayout

newPlot :: String -> Plotly -> Html ()
newPlot divNm (Plotly trs lay) =
  let trJSON = decodeUtf8 $ toStrict $ encode trs
      layoutJSON = {-case mlay of
                     Nothing -> ""
                     Just lay -> -} ","<>(decodeUtf8 $ toStrict $ encode lay)
  in script_ ("Plotly.newPlot('"<>pack divNm<>"', "<>trJSON<>layoutJSON<>", {displayModeBar: false});")
