{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, TemplateHaskell, DeriveGeneric #-}

module Graphics.Echarts where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson hiding (Series)
import Data.Aeson.Types hiding (Series)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Lucid
import Lens.Micro.TH
import GHC.Generics

jsonOptions :: Maybe String -> Options
jsonOptions mprefix = defaultOptions { omitNothingFields = True, fieldLabelModifier = unPrefix . unLens }
  where
    unLens :: String -> String
    unLens ('_':s) = s
    unLens s = s
    unPrefix :: String -> String
    unPrefix = maybe id (\pre s -> fromMaybe s $ stripPrefix pre s) mprefix

echartsCDN :: Monad m => HtmlT m ()
echartsCDN = script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/echarts/3.6.2/echarts.js"] ""

dropInitial :: String -> String -> String
dropInitial s s' = case stripPrefix s s' of
                    Nothing -> s'
                    Just s'' -> s''

data TextStyle = TextStyle {
  _textStyle_fontSize :: Maybe Integer
} deriving (Show, Generic)

makeLenses ''TextStyle

instance ToJSON TextStyle where
  toJSON = genericToJSON $ jsonOptions (Just "textStyle_")

data LabelPosition = Outside | Inside | Inner | Center deriving Show

instance ToJSON LabelPosition where
  toJSON = toJSON . map toLower . show

-- | Labels can be normal or emphasis (see LineStyle definition)
data NormalLabel = NormalLabel {
  _normal_show      :: Maybe Bool,
  _normal_position  :: Maybe LabelPosition,
  _normal_textStyle :: Maybe TextStyle
} deriving (Show, Generic)

makeLenses ''NormalLabel

defNormalLabel :: NormalLabel
defNormalLabel = NormalLabel Nothing Nothing Nothing

instance ToJSON NormalLabel where
  toJSON = genericToJSON $ jsonOptions (Just "normal_")

data EmphasisLabel = EmphasisLabel {
  _emphasis_show      :: Maybe Bool,
  _emphasis_position  :: Maybe LabelPosition,
  _emphasis_textStyle :: Maybe TextStyle
} deriving (Show, Generic)

makeLenses ''EmphasisLabel

defEmphasisLabel :: EmphasisLabel
defEmphasisLabel = EmphasisLabel Nothing Nothing Nothing

instance ToJSON EmphasisLabel where
  toJSON = genericToJSON $ jsonOptions (Just "emphasis_")

data Label = Label {
  _label_normal :: Maybe NormalLabel,
  _label_emphasis :: Maybe EmphasisLabel
} deriving (Show, Generic)

makeLenses ''Label

defLabel :: Label
defLabel = Label Nothing Nothing

instance ToJSON Label where
  toJSON = genericToJSON $ jsonOptions (Just "label_")


-- | For line styles, default values are:
--   width = 1, curveness = 0, opacity = 0.5
data NormalLineStyle = NormalLineStyle {
  _normal_width     :: Maybe Integer,
  _normal_curveness :: Maybe Double,
  _normal_opacity   :: Maybe Double
} deriving (Show, Generic)

makeLenses ''NormalLineStyle

defNormalLineStyle :: NormalLineStyle
defNormalLineStyle = NormalLineStyle Nothing Nothing Nothing

instance ToJSON NormalLineStyle where
  toJSON = genericToJSON $ jsonOptions (Just "normal_")

data EmphasisLineStyle = EmphasisLineStyle {
  _emphasis_width     :: Maybe Integer,
  _emphasis_curveness :: Maybe Double,
  _emphasis_opacity   :: Maybe Double
} deriving (Show, Generic)

makeLenses ''EmphasisLineStyle

defEmphasisLineStyle :: EmphasisLineStyle
defEmphasisLineStyle = EmphasisLineStyle Nothing Nothing Nothing

instance ToJSON EmphasisLineStyle where
  toJSON = genericToJSON $ jsonOptions (Just "emphasis_")

-- | Normal is the style by default, emphasis is the style when
-- | an object is highlighted (e.g. hovered over)
data LineStyle = LineStyle {
  _linestyle_normal   :: Maybe NormalLineStyle,
  _linestyle_emphasis :: Maybe EmphasisLineStyle
} deriving (Show, Generic)

makeLenses ''LineStyle

defLineStyle :: LineStyle
defLineStyle = LineStyle Nothing Nothing

instance ToJSON LineStyle where
  toJSON = genericToJSON $ jsonOptions (Just "linestyle_")


data DataValue = PieValue Double
               | ScatterValue [Double] deriving (Show, Generic, Eq)

makeLenses ''DataValue

instance ToJSON DataValue where
  toJSON (PieValue x) = toJSON x
  toJSON (ScatterValue xs) = toJSON xs

-- | A node is called Data in this library
data Data = Data {
  _data_name  :: Maybe Text,
  _data_value :: Maybe DataValue,   -- for pie charts and scatters
  _data_x     :: Maybe Double,   -- x and y positions for node and edge graph
  _data_y     :: Maybe Double
} deriving (Show, Generic, Eq)

makeLenses ''Data

instance ToJSON Data where
  toJSON = genericToJSON $ jsonOptions (Just "data_")

-- | A link is an edge between two nodes
data Link = Link {
  _link_source    :: Text,
  _link_target    :: Text,
  _link_label     :: Maybe Label,
  _link_lineStyle :: Maybe LineStyle
} deriving (Show, Generic)

makeLenses ''Link

instance ToJSON Link where
  toJSON = genericToJSON $ jsonOptions (Just "link_")


-- | The type of graph, Graph is for a node and edges graph
data SeriesType = Graph | Pie | Scatter deriving Show

instance ToJSON SeriesType where
  toJSON = toJSON . map toLower . show

-- | A Series is the component of the plot
data Series = Series {
  _series_name      :: Maybe Text,
  _series_type      :: SeriesType,      -- type of graph (pie, scatter, graph)
  _series_symbolSize       :: Maybe Integer,         -- size of nodes
  _series_roam             :: Maybe Bool,            -- roam=true allows zooming and scaling
  _series_label     :: Maybe Label,           -- global specifications for labels (for nodes)
  _series_edgeSymbol       :: Maybe [Text],          -- symbol for each ends in the format [source,target]
  _series_edgeSymbolSize   :: Maybe [Integer],       -- size of edge symbols, same format
  _series_edgeLabel        :: Maybe Label,           -- global specifications for edge labels
  _series_lineStyle :: Maybe LineStyle,        -- global specifications for edge style
  _series_data      :: Maybe [Data],          -- nodes
  _series_links     :: Maybe [Link],          -- edges
  _series_xAxisIndex :: Maybe Integer,
  _series_yAxisIndex :: Maybe Integer,
  _series_layout :: Maybe Text,
  -- Pie chart
  _series_radius    :: Maybe [Text],           -- [inner_radius,outer_radius], default = [0,"75%"]]
  _series_avoidLabelOverlap :: Maybe Bool       -- true = labels do not overlap
} deriving (Show, Generic)

makeLenses ''Series

mkSeries :: SeriesType -> Series
mkSeries tt = Series Nothing tt Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkScatter :: Series
mkScatter = mkSeries Scatter

mkPie :: Series
mkPie = mkSeries Pie

mkGraph :: Series
mkGraph = mkSeries Graph

instance ToJSON Series where
  toJSON = genericToJSON $ jsonOptions (Just "series_")

-- good values for radii
defRadius, thinRadius, thickRadius :: [Text]
defRadius   = ["0%","75%"]
thinRadius  = ["50%","70%"]
thickRadius = ["20%","70%"]


data Title = Title {
  _title_text :: Text
} deriving (Show, Generic)

makeLenses ''Title

instance ToJSON Title where
  toJSON = genericToJSON $ jsonOptions (Just "title_")

-- | Specifies the grid size if you want multiple series,
--   these values take percentage as Text, e.g. "70%"
data Grid = Grid {
  _grid_left   :: Maybe Text,
  _grid_top    :: Maybe Text,
  _grid_width  :: Maybe Text,
  _grid_height :: Maybe Text
} deriving (Show, Generic)

makeLenses ''Grid

defGrid :: Grid
defGrid = Grid Nothing Nothing Nothing Nothing

instance ToJSON Grid where
  toJSON = genericToJSON $ jsonOptions (Just "grid_")

-- | Tooltip that displays when an object is hovered over.
--   full documentation for formatter can be found at:
--   https://ecomfe.github.io/echarts-doc/public/en/option.html#series-pie.tooltip.formatter
data Tooltip = Tooltip {
  _tooltip_trigger   :: Maybe Text,
  _tooltip_formatter :: Maybe Text
} deriving (Show, Generic)

makeLenses ''Tooltip

defTooltip :: Tooltip
defTooltip = Tooltip Nothing Nothing

instance ToJSON Tooltip where
  toJSON = genericToJSON $ jsonOptions (Just "tooltip_")


data Axis = Axis {
  _axis_gridIndex :: Integer,
  _axis_min       :: Maybe Integer,
  _axis_max       :: Maybe Integer
} deriving (Show, Generic)

makeLenses ''Axis

defAxis :: Axis
defAxis = Axis 0 Nothing Nothing

instance ToJSON Axis where
  toJSON = genericToJSON $ jsonOptions (Just "axis_")


data Orientation = Horizontal | Vertical deriving Show

instance ToJSON Orientation where
  toJSON = toJSON . map toLower .  show

data HorizontalPosition = XLeft | XCenter | XRight deriving (Show, Generic)

instance ToJSON HorizontalPosition where
  toJSON = toJSON . drop 2 . camelTo2 '_' . show

data VerticalPosition = YTop | YMiddle | YBottom deriving (Show, Generic)

instance ToJSON VerticalPosition where
  toJSON = toJSON . drop 2 . camelTo2 '_' . show

data LegendData = LegendData {
  _legendData_name :: Maybe Text
} deriving (Show, Generic)

makeLenses ''LegendData

instance ToJSON LegendData where
  toJSON = genericToJSON $ jsonOptions (Just "legendData_")

-- | Legend of graph, enter data names into legend_data
data Legend = Legend {
  _legend_orient  :: Maybe Orientation,
  _legend_x       :: Maybe HorizontalPosition,
  _legend_y       :: Maybe VerticalPosition,
  _legend_data    :: Maybe [LegendData]
} deriving (Show, Generic)

makeLenses ''Legend

defLegend :: Legend
defLegend = Legend Nothing Nothing Nothing Nothing

instance ToJSON Legend where
  toJSON = genericToJSON $ jsonOptions (Just "legend_")

data EchartsOptions = EchartsOptions {
  _options_title   :: Title,
  _options_grid    :: Maybe [Grid],
  _options_tooltip :: Maybe Tooltip,
  _options_xAxis   :: Maybe [Axis],
  _options_yAxis   :: Maybe [Axis],
  _options_series  :: [Series],
  _options_legend  :: Maybe Legend
} deriving (Show, Generic)

makeLenses ''EchartsOptions

instance ToJSON EchartsOptions where
  toJSON = genericToJSON $ jsonOptions (Just "options_")

mkOptions :: Text -> [Series] -> EchartsOptions
mkOptions title series = EchartsOptions (Title title) (Just [defGrid]) (Just defTooltip) Nothing Nothing series (Just defLegend)

runEcharts :: Text -> EchartsOptions -> Text
runEcharts element options = T.unlines [
  "",
  "   var myChart = echarts.init(document.getElementById('"<>element<>"'));",
  "   var option = " <> (decodeUtf8 $ toStrict $ encode options),
  "   myChart.setOption(option);"
  ]
