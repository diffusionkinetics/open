{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, TemplateHaskell, DeriveGeneric #-}

module Graphics.Echarts where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Aeson.Types
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


-- placeholder
type EchartsOptions = Text

-- A link is an edge between two nodes
data Link = Link {
  _link_source :: Text,
  _link_target :: Text
} deriving (Show, Generic)

makeLenses ''Link

instance ToJSON Link where
  toJSON = genericToJSON $ jsonOptions (Just "link_")

-- A node is called Data in this library
data Data = Data {
  _node_name :: Text,
  _node_x    :: Integer,
  _node_y    :: Integer
} deriving (Show, Generic)

makeLenses ''Data

instance ToJSON Data where
  toJSON = genericToJSON $ jsonOptions (Just "data_")

data Layout = Layout {
    _layout_name :: Text
} deriving (Show, Generic)

makeLenses ''Layout

instance ToJSON Layout where
  toJSON = genericToJSON $ jsonOptions (Just "layout_")



runEcharts :: Text -> EchartsOptions -> Text
runEcharts element options = T.unlines [
  "",
  "$(function(){",
  "// based on prepared DOM, initialize echarts instance ",
        "var myChart = echarts.init(document.getElementById('"<>element<>"'));",
        "var option = " <>(decodeUtf8 $ toStrict $ encode options),
  "     myChart.setOption(option);",
  " })"
  ]
