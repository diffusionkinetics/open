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
  _link_source :: Text,
  _link_target :: Text
} deriving (Show, Generic)

makeLenses ''Link

instance ToJSON Link where
  toJSON = genericToJSON $ jsonOptions (Just "link_")

-- | A Series is the component of the plot
data Series = Series {
  _series_type             :: Text,
  -- _symbolSize       :: Integer,         -- size of nodes
  -- _roam             :: Bool,            -- roam=true allows zooming and scaling
  -- _series_label     :: Label,           -- global specifications for labels (for nodes)
  -- _edgeSymbol       :: [Text],          -- symbol for each ends in the format [source,target]
  -- _edgeSymbolSize   :: [Integer],       -- size of edge symbols, same format
  -- _edgeLabel        :: Label,           -- global specifications for edge labels
  -- _series_lineStyle :: LineStyle        -- global specifications for edge style
  _series_data             :: [Data],          -- nodes
  _series_links            :: [Link]          -- edges
} deriving (Show, Generic)

makeLenses ''Series

instance ToJSON Series where
  toJSON = genericToJSON $ jsonOptions (Just "series_")

-- placeholder
data EchartsOptions = EchartsOptions {
  options_series :: Series
} deriving (Show, Generic)

makeLenses ''EchartsOptions

instance ToJSON EchartsOptions where
  toJSON = genericToJSON $ jsonOptions (Just "options_")


mkOptions :: [Data] -> [Link] -> EchartsOptions
mkOptions nodes edges = EchartsOptions $ Series "graph" nodes edges

runEcharts :: Text -> Text -> [Data] -> [Link] -> Text
runEcharts element s_type nodes links = T.unlines [
  "",
  "$(function(){",
  "   var myChart = echarts.init(document.getElementById('"<>element<>"'));",
  "   myChart.setOption({",
  "     series : [{",
  "       type: " <> (decodeUtf8 $ toStrict $ encode s_type) <>",",
  "       data: " <> (decodeUtf8 $ toStrict $ encode nodes) <>",",
  "       links: " <> (decodeUtf8 $ toStrict $ encode links),
  "     }]",
  "   });",
  " })"
  ]
