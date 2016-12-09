{-# LANGUAGE DeriveGeneric, OverloadedStrings,FlexibleInstances, TemplateHaskell #-}

{-|

This module defines datatypes that can be used to generate [Plotly.js](https://plot.ly/javascript/)
plots via their JSON values. The interface encourages the use of
lenses. Every trace on a plot is defined by a `Trace` type value, the
construction of which is the central goal of this module.

Example scatter plot of the Iris dataset:

@
import Graphics.Plotly
import Numeric.Dataset.Iris

tr :: Trace
tr = scatter & x ?~ map sepalLength iris
             & y ?~ map sepalWidth iris
             & marker ?~ (defMarker & markercolor ?~ catColors (map irisClass irisd))
             & mode ?~ [Markers]
@

Horizontal bars:

@
hbarData :: [(Text, Double)]
hbarData = [(\"Simon\", 14.5), (\"Joe\", 18.9), (\"Dorothy\", 16.2)]

hbarsTrace :: Trace
hbarsTrace = bars & ytext ?~ map fst hbarData
                  & x ?~ map snd hbarData
                  & orientation ?~ Horizontal
@

see Graphics.Plotly.Lucid for helper functions that turn traces into HTML.

-}

module Graphics.Plotly where

import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import Data.List (intercalate, nub, findIndex)
import Data.Monoid ((<>))
import Data.Maybe (fromJust)
import Data.Text (Text)

import GHC.Generics
import Lens.Micro.TH

import Graphics.Plotly.Utils

-- * Traces

-- |How should traces be drawn? (lines or markers)
data Mode = Markers | Lines deriving Show

instance {-# OVERLAPS #-} ToJSON [Mode] where
  toJSON = toJSON . intercalate "+" . map (map toLower . show)

-- | What kind of plot type are we building - scatter (inluding line plots) or bars?
data TraceType = Scatter | Bar deriving Show

instance ToJSON TraceType where
  toJSON = toJSON . map toLower . show


-- | A color specification, either as a concrete RGB/RGBA value or a color per point.
data Color = RGBA Int Int Int Int -- ^ use this RGBA color for every point in the trace
           | RGB Int Int Int -- ^ use this RGB color for every point in the trace
           | ColIx Int  -- ^ use a different color index for each point

instance ToJSON Color where
  toJSON (RGB r g b) = toJSON $ "rgb("<>show r<>","<>show g<>","<>show b<>")"
  toJSON (RGBA r g b a) = toJSON $ "rgba("<>show r<>","<>show g<>","<>show b<>","<> show a<>")"
  toJSON (ColIx cs) = toJSON cs

-- | Assign colors based on any categorical value
catColors :: Eq a => [a] -> ListOrElem Value
catColors xs =
  let vals = nub xs
      f x = fromJust $ findIndex (==x) vals
  in List $ map (toJSON . ColIx . f) xs

-- | Different types of markers
data Symbol = Circle | Square | Diamond | Cross deriving (Show, Eq)

instance ToJSON Symbol where
  toJSON = toJSON . map toLower . show

data ListOrElem a = List [a] | All a deriving Eq

instance ToJSON a => ToJSON (ListOrElem a) where
  toJSON (List xs) = toJSON xs
  toJSON (All x) = toJSON x

-- | Marker specification
data Marker = Marker
  { _size :: Maybe (ListOrElem Value)
  , _markercolor :: Maybe (ListOrElem Value)
  , _symbol :: Maybe Symbol
  , _opacity :: Maybe Double
  } deriving (Generic, Eq)

makeLenses ''Marker

instance ToJSON Marker where
  toJSON = genericToJSON jsonOptions {fieldLabelModifier = rename "markercolor" "color" . unLens}

-- | default marker specification
defMarker :: Marker
defMarker  = Marker Nothing Nothing Nothing Nothing


-- | Dash type specification
data Dash = Solid | Dashdot | Dot deriving Show

instance ToJSON Dash where
  toJSON = toJSON . map toLower . show

-- | Horizontal or Vertical orientation of bars
data Orientation = Horizontal | Vertical

instance ToJSON Orientation where
  toJSON Horizontal = "h"
  toJSON Vertical = "v"

-- | Are we filling area plots from the zero line or to the next Y value?
data Fill = ToZeroY | ToNextY deriving Show

instance ToJSON Fill where
  toJSON = toJSON . map toLower . show

-- | line specification
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

-- | A `Trace` is the component of a plot. Multiple traces can be superimposed.
data Trace = Trace
  { _x :: Maybe [Value] -- ^ x values, as numbers
  , _y :: Maybe [Value] -- ^ y values, as numbers
  , _mode :: Maybe [Mode] -- ^ select one or two modes.
  , _name :: Maybe Text -- ^ name of this trace, for legend
  , _text :: Maybe [Text]
  , _tracetype :: TraceType
  , _marker :: Maybe Marker
  , _line :: Maybe Line
  , _fill :: Maybe Fill
  , _orientation :: Maybe Orientation
  } deriving Generic

makeLenses ''Trace

-- |an empty scatter plot
scatter :: Trace
scatter = Trace Nothing Nothing Nothing Nothing Nothing Scatter Nothing Nothing Nothing Nothing

-- |an empty bar plot
bars :: Trace
bars = Trace Nothing Nothing Nothing Nothing Nothing Bar Nothing Nothing Nothing Nothing


instance ToJSON Trace where
  toJSON = genericToJSON jsonOptions {fieldLabelModifier = rename "tracetype" "type" . unLens}

-- |Options for axes
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

-- * Layouts

-- | How different bar traces be superimposed? By grouping or by stacking?
data Barmode = Stack | Group deriving Show

instance ToJSON Barmode where
  toJSON = toJSON . map toLower . show

-- |Options for Margins.
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

-- | some good values for margins
thinMargins, titleMargins :: Margin
thinMargins = Margin 50 25 30 10 4
titleMargins = Margin 50 25 30 40 4


-- |options for the layout of the whole plot
data Layout = Layout
  { _xaxis :: Maybe Axis
  , _yaxis :: Maybe Axis
  , _title :: Maybe Text
  , _showlegend :: Maybe Bool
  , _height :: Maybe Int
  , _width :: Maybe Int
  , _barmode :: Maybe Barmode
  , _margin :: Maybe Margin
  } deriving Generic

makeLenses ''Layout

-- |a defaultlayout
defLayout :: Layout
defLayout = Layout Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON Layout where
  toJSON = genericToJSON jsonOptions

-- * Plotly

-- | A helper record which represents the whole plot
data Plotly = Plotly
  { _elemid :: Text
  , _traces :: [Trace]
  , _layout :: Layout
  }

makeLenses ''Plotly

-- | helper function for building the plot.
plotly :: Text -> [Trace] -> Plotly
plotly idnm trs = Plotly idnm trs defLayout
