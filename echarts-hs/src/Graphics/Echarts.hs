{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, TemplateHaskell, DeriveGeneric #-}

module Graphics.Echarts where

import Data.Text (Text)
import qualified Data.Text as T
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


data TextStyle = TextStyle {
  _textStyle_fontSize :: Integer
} deriving (Show, Generic)

makeLenses ''TextStyle

instance ToJSON TextStyle where
  toJSON = genericToJSON $ jsonOptions (Just "textStyle_")

-- | Labels can be normal or emphasis (see LineStyle definition)
data NormalLabelData = NormalLabelData {
  _normal_show      :: Maybe Bool,
  _normal_textStyle :: Maybe TextStyle
} deriving (Show, Generic)

makeLenses ''NormalLabelData

instance ToJSON NormalLabelData where
  toJSON = genericToJSON $ jsonOptions (Just "normal_")

data EmphasisLabelData = EmphasisLabelData {
  _emphasis_show      :: Maybe Bool,
  _emphasis_textStyle :: Maybe TextStyle
} deriving (Show, Generic)

makeLenses ''EmphasisLabelData

instance ToJSON EmphasisLabelData where
  toJSON = genericToJSON $ jsonOptions (Just "emphasis_")

data Label = NormalLabel { _label_normal :: NormalLabelData }
           | EmphasisLabel { _label_emphasis :: EmphasisLabelData}
  deriving (Show, Generic)

makeLenses ''Label

instance ToJSON Label where
  toJSON = genericToJSON $ jsonOptions (Just "label_")


-- | For line styles, default values are:
--   width = 1, curveness = 0, opacity = 0.5
data NormalLineStyleData = NormalLineStyleData {
  _normal_width     :: Integer,
  _normal_curveness :: Double,
  _normal_opacity   :: Double
} deriving (Show, Generic)

makeLenses ''NormalLineStyleData

instance ToJSON NormalLineStyleData where
  toJSON = genericToJSON $ jsonOptions (Just "normal_")

data EmphasisLineStyleData = EmphasisLineStyleData {
  _emphasis_width     :: Integer,
  _emphasis_curveness :: Double,
  _emphasis_opacity   :: Double
} deriving (Show, Generic)

makeLenses ''EmphasisLineStyleData

instance ToJSON EmphasisLineStyleData where
  toJSON = genericToJSON $ jsonOptions (Just "emphasis_")

-- | Normal is the style by default, emphasis is the style when
-- | an object is highlighted (e.g. hovered over)
data LineStyle = NormalLineStyle { _linestyle_normal :: NormalLineStyleData }
               | EmphasisLineStyle { _linestyle_emphasis :: EmphasisLineStyleData}
  deriving (Show, Generic)

makeLenses ''LineStyle

instance ToJSON LineStyle where
  toJSON = genericToJSON $ jsonOptions (Just "linestyle_")

-- | A node is called Data in this library
data Data = Data {
  _data_name :: Text,
  _data_x    :: Integer,
  _data_y    :: Integer
} deriving (Show, Generic)

makeLenses ''Data

instance ToJSON Data where
  toJSON = genericToJSON $ jsonOptions (Just "data_")

-- | A link is an edge between two nodes
data Link = Link {
  _link_source    :: Text,
  _link_target    :: Text,
  _link_lineStyle :: Maybe LineStyle
} deriving (Show, Generic)

makeLenses ''Link

instance ToJSON Link where
  toJSON = genericToJSON $ jsonOptions (Just "link_")

-- | A Series is the component of the plot
data Series = Series {
  _series_type             :: Text,
  _symbolSize       :: Integer,         -- size of nodes
  _roam             :: Bool,            -- roam=true allows zooming and scaling
  _series_label     :: Label,           -- global specifications for labels (for nodes)
  _edgeSymbol       :: [Text],          -- symbol for each ends in the format [source,target]
  _edgeSymbolSize   :: [Integer],       -- size of edge symbols, same format
  _edgeLabel        :: Label,           -- global specifications for edge labels
  _series_lineStyle :: LineStyle,        -- global specifications for edge style
  _series_data             :: [Data],          -- nodes
  _series_links            :: [Link]          -- edges
} deriving (Show, Generic)

makeLenses ''Series

instance ToJSON Series where
  toJSON = genericToJSON $ jsonOptions (Just "series_")

data Tooltip = Tooltip {
  _tooltip_trigger   :: Maybe Text,
  _tooltip_formatter :: Maybe Text
} deriving (Show, Generic)

makeLenses ''Tooltip

instance ToJSON Tooltip where
  toJSON = genericToJSON $ jsonOptions (Just "tooltip_")

defTooltip :: Tooltip
defTooltip = Tooltip (Just "item") Nothing

data EchartsOptions = EchartsOptions {
  options_tooltip :: Tooltip,
  options_series  :: Series
} deriving (Show, Generic)

makeLenses ''EchartsOptions

instance ToJSON EchartsOptions where
  toJSON = genericToJSON $ jsonOptions (Just "options_")

runEcharts :: Text -> EchartsOptions -> Text
runEcharts element options = T.unlines [
  "",
  "$(function(){",
  "   var myChart = echarts.init(document.getElementById('"<>element<>"'));",
  "   var option = " <> (decodeUtf8 $ toStrict $ encode options),
  "   myChart.setOption(option);",
  " })"
  ]
