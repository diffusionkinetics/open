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

module Graphics.Plotly.Base where

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
data Mode = Markers | Lines | ModeText deriving Show

instance {-# OVERLAPS #-} ToJSON [Mode] where
  toJSON = toJSON . intercalate "+" . map (map toLower . dropInitial "Mode" . show)

-- | What kind of plot type are we building - scatter (inluding line plots) or bars?
data TraceType  = Scatter
                | Scatter3D
                | Bar
                | Box
                | Mesh3D
                | Pie
                | Contour
                deriving Show

instance ToJSON TraceType where
  toJSON = toJSON . map toLower . show


-- | A color specification, either as a concrete RGB/RGBA value or a color per point.
data Color = ColRGBA Int Int Int Int -- ^ use this RGBA color for every point in the trace
           | ColRGB Int Int Int -- ^ use this RGB color for every point in the trace
           | ColIx Int  -- ^ use a different color index for each point

instance Eq Color where
  (ColRGBA r0 g0 b0 a0) == (ColRGBA r1 g1 b1 a1) = (r0,g0,b0,a0) == (r1,g1,b1,a1)
  (ColRGB  r0 g0 b0)    == (ColRGB  r1 g1 b1)    = (r0,g0,b0)    == (r1,g1,b1)
  (ColIx   i0)          == (ColIx   i1)          = i0 == i1

  (ColRGBA r0 g0 b0 1)  == (ColRGB  r1 g1 b1)    = (r0,g0,b0) == (r1,g1,b1)
  (ColRGB  r0 g0 b0)    == (ColRGBA r1 g1 b1 1)  = (r0,g0,b0) == (r1,g1,b1)

  _ == _ = False

instance ToJSON Color where
  toJSON (ColRGB r g b) = toJSON $ "rgb("<>show r<>","<>show g<>","<>show b<>")"
  toJSON (ColRGBA r g b a) = toJSON $ "rgba("<>show r<>","<>show g<>","<>show b<>","<> show a<>")"
  toJSON (ColIx cs) = toJSON cs

-- | Assign colors based on any categorical value
catColors :: Eq a => [a] -> ListOrElem Value
catColors xs =
  let vals = nub xs
      f x = fromJust $ findIndex (==x) vals
  in List $ map (toJSON . ColIx . f) xs

-- | Different types of markers
data Symbol = Circle | Square | Diamond | Cross | CustomSymbol Text deriving (Show, Eq)

instance ToJSON Symbol where
  toJSON (CustomSymbol t) = toJSON t
  toJSON s = toJSON . map toLower . show $ s

data ListOrElem a = List [a] | All a deriving Eq

instance ToJSON a => ToJSON (ListOrElem a) where
  toJSON (List xs) = toJSON xs
  toJSON (All x) = toJSON x

data Sizemode = Diameter | Area deriving (Show, Eq)

instance ToJSON Sizemode where
  toJSON = toJSON . map toLower . show

-- | Marker line specification
data MarkerLine = MarkerLine
  { _markerlinewidth :: Maybe (ListOrElem Double)
  , _markerlinecolor :: Maybe (ListOrElem Value)
  } deriving (Generic, Eq)

makeLenses ''MarkerLine

instance ToJSON MarkerLine where
  toJSON = genericToJSON jsonOptions {fieldLabelModifier = dropInitial "markerline" . unLens}

-- | default marker line specification
defMarkerLine :: MarkerLine
defMarkerLine = MarkerLine Nothing Nothing

-- | Marker specification
data Marker = Marker
  { _size :: Maybe (ListOrElem Value)
  , _sizeref :: Maybe Value
  , _sizeMode :: Maybe Sizemode
  , _markercolor :: Maybe (ListOrElem Value)
  , _markercolors :: Maybe (ListOrElem Value) -- for pie charts
  , _symbol :: Maybe (ListOrElem Symbol)
  , _opacity :: Maybe Double
  , _markerline :: Maybe MarkerLine
  } deriving (Generic, Eq)

makeLenses ''Marker

instance ToJSON Marker where
  toJSON = genericToJSON jsonOptions {fieldLabelModifier = dropInitial "marker" . unLens}

-- | default marker specification
defMarker :: Marker
defMarker  = Marker Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


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
data Fill = FillNone | ToZeroY | ToNextY | ToZeroX | ToNextX | ToSelf | ToNext deriving Show

instance ToJSON Fill where
  toJSON = toJSON . map toLower . dropInitial "Fill" . show


data LineShape = Linear | Spline | Hv | Hvh | Vh | Vhv deriving Show

instance ToJSON LineShape where
  toJSON = toJSON . map toLower . show

-- | line specification
data Line = Line
  { _linewidth :: Maybe Double
  , _linecolor :: Maybe Color
  , _lineshape :: Maybe LineShape
  , _dash :: Maybe Dash
  } deriving Generic

makeLenses ''Line

instance ToJSON Line where
  toJSON = genericToJSON jsonOptions { fieldLabelModifier = dropInitial "line" . unLens}

defLine :: Line
defLine = Line Nothing Nothing Nothing Nothing

data HoverElem = HoverX | HoverY | HoverZ | HoverText | HoverName
  deriving (Generic, Show)

data HoverInfo = HoverPlus [HoverElem] | HoverAll | HoverNone | HoverSkip
  deriving (Generic, Show)

instance ToJSON HoverInfo where
  toJSON (HoverPlus elems) = toJSON . intercalate "+" $ (map toLower . dropInitial "Hover" . show) <$> elems
  toJSON x                 = toJSON . map toLower . dropInitial "Hover" $ show x

data HoverOn = HoverPoints | HoverFills deriving (Generic, Show)

instance {-# OVERLAPS #-} ToJSON [HoverOn] where
  toJSON = toJSON . intercalate "+" . map (map toLower . dropInitial "Hover" . show)

data TextPosition
  = TopLeft    | TopCenter    | TopRight
  | MiddleLeft | MiddleCenter | MiddleRight
  | BottomLeft | BottomCenter | BottomRight
  deriving (Generic, Show)

instance ToJSON TextPosition where
  toJSON = toJSON . camelTo2 ' ' . show

-- | A `Trace` is the component of a plot. Multiple traces can be superimposed.
data Trace = Trace
  { _x :: Maybe [Value] -- ^ x values, as numbers
  , _y :: Maybe [Value] -- ^ y values, as numbers
  , _z :: Maybe [Value] -- ^ z values, as numbers
  , _values :: Maybe [Value] -- values for pie chart
  , _labels :: Maybe [Text] -- labels for pie chart
  , _hole :: Maybe Value -- pie chart hole property
  , _mode :: Maybe [Mode] -- ^ select one or two modes.
  , _name :: Maybe Text -- ^ name of this trace, for legend
  , _text :: Maybe [Text]
  , _textposition :: Maybe TextPosition
  , _tracetype :: TraceType
  , _marker :: Maybe Marker
  , _line :: Maybe Line
  , _fill :: Maybe Fill
  , _orientation :: Maybe Orientation
  , _visible :: Maybe Value
  , _traceshowlegend :: Maybe Bool
  , _legendgroup :: Maybe Text
  , _customdata :: Maybe [Value]
  , _hoverinfo :: Maybe HoverInfo
  , _hovertext :: Maybe (ListOrElem Text)
  , _hoveron :: Maybe [HoverOn]
  , _connectgaps :: Maybe Bool

  -- Pie
  , _sort :: Maybe Bool

  -- 3D mesh
  , _i :: Maybe [Int] -- ^ i values, as ints
  , _j :: Maybe [Int] -- ^ j values, as ints
  , _k :: Maybe [Int] -- ^ k values, as ints
  , _tracecolor :: Maybe Color
  , _traceopacity :: Maybe Double

  -- Sub-plots
  , _tracexaxis :: Maybe Text -- ^ X-axis name
  , _traceyaxis :: Maybe Text -- ^ Y-axis name
  } deriving Generic

makeLenses ''Trace

mkTrace :: TraceType -> Trace
mkTrace tt = Trace Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing tt Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- |an empty scatter plot
scatter :: Trace
scatter = mkTrace Scatter

-- |an empty 3D scatter plot
scatter3d :: Trace
scatter3d = mkTrace Scatter3D

-- |an empty bar plot
bars :: Trace
bars = mkTrace Bar

-- |an empty box plot
box :: Trace
box = mkTrace Box

-- |an empty 3D mesh plot
mesh3d :: Trace
mesh3d = mkTrace Mesh3D

-- |an empty 3D mesh plot
contour :: Trace
contour = mkTrace Contour

-- | an empty pie chart
pie :: Trace
pie = mkTrace Pie

instance ToJSON Trace where
  toJSON = genericToJSON jsonOptions {fieldLabelModifier = renamer}
    where renamer = dropInitial "trace" . unLens

data AxisType = Log | Date | Category deriving Show

instance ToJSON AxisType where
  toJSON = toJSON . map toLower . show

-- |Options for axes
data Axis = Axis
  { _range :: Maybe (Double,Double)
  , _axistype :: Maybe AxisType
  , _axistitle :: Maybe Text
  , _showgrid :: Maybe Bool
  , _zeroline :: Maybe Bool
  , _axisvisible :: Maybe Bool
  , _tickvals :: Maybe [Value]
  , _ticktext :: Maybe [Text]
  , _domain :: Maybe (Double,Double)
  } deriving Generic

makeLenses ''Axis

instance ToJSON Axis where
  toJSON = genericToJSON jsonOptions {fieldLabelModifier = dropInitial "axis" . unLens}

defAxis :: Axis
defAxis = Axis Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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

-- | Options for Fonts.
data Font = Font
  { _fontfamily :: Maybe Text
  , _fontsize   :: Maybe Double
  , _fontcolor  :: Maybe Color
  } deriving Generic

makeLenses ''Font

instance ToJSON Font where
  toJSON = genericToJSON jsonOptions { fieldLabelModifier = dropInitial "font" . unLens}

defFont :: Font
defFont = Font Nothing Nothing Nothing

data Align
  = AlignLeft | AlignCenter | AlignRight
  deriving (Generic, Show)

instance ToJSON Align where
  toJSON = toJSON . map toLower . dropInitial "Align" . show

-- | Options for annotations
data Annotation = Annotation
  { _annotationvisible     :: Maybe Bool
  , _annotationtext        :: Maybe Text
  , _annotationfont        :: Maybe Font
  , _annotationwidth       :: Maybe Double
  , _annotationheight      :: Maybe Double
  , _annotationopacity     :: Maybe Double
  , _annotationalign       :: Maybe Align
  , _annotataonbgcolor     :: Maybe Color
  , _annotationbordercolor :: Maybe Color
  , _annotationshowarrow   :: Maybe Bool
  , _annotationx           :: Maybe Value
  , _annotationxref        :: Maybe Text -- ^ "paper" or X-axis name
  , _annotationxshift      :: Maybe Double
  , _annotationy           :: Maybe Value
  , _annotationyref        :: Maybe Text -- ^ "paper" or Y-axis name
  , _annotationyshift      :: Maybe Double
  } deriving Generic

makeLenses ''Annotation

defAnnotation :: Annotation
defAnnotation = Annotation Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON Annotation where
  toJSON = genericToJSON jsonOptions {fieldLabelModifier = dropInitial "annotation" . unLens}

-- |options for the layout of the whole plot
data Layout = Layout
  { _xaxis  :: Maybe Axis
  , _xaxis2 :: Maybe Axis
  , _xaxis3 :: Maybe Axis
  , _xaxis4 :: Maybe Axis
  , _yaxis  :: Maybe Axis
  , _yaxis2 :: Maybe Axis
  , _yaxis3 :: Maybe Axis
  , _yaxis4 :: Maybe Axis
  , _zaxis  :: Maybe Axis
  , _title  :: Maybe Text
  , _titlefont :: Maybe Font
  , _showlegend :: Maybe Bool
  , _height :: Maybe Int
  , _width :: Maybe Int
  , _barmode :: Maybe Barmode
  , _margin :: Maybe Margin
  , _font :: Maybe Font
  , _annotations :: Maybe [Annotation]
  } deriving Generic

makeLenses ''Layout

-- |a defaultlayout
defLayout :: Layout
defLayout = Layout Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON Layout where
  toJSON = genericToJSON jsonOptions

-- * Plotly

-- | A helper record which represents the whole plot
data Plotly = Plotly
  { _elemid :: Text
  , _traces :: [Trace]
  , _layout :: Layout
  } deriving Generic

instance ToJSON Plotly where
  toJSON = genericToJSON jsonOptions

makeLenses ''Plotly

-- | helper function for building the plot.
plotly :: Text -> [Trace] -> Plotly
plotly idnm trs = Plotly idnm trs defLayout
