{-# LANGUAGE OverloadedStrings, StandaloneDeriving,  DeriveGeneric #-}

module Lucid.Leaflet where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Lucid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Lucid.PreEscaped
import Data.Monoid
import GHC.Generics

leafletCDN :: Monad m => HtmlT m ()
leafletCDN 
  =  scriptSrc "https://unpkg.com/leaflet@1.2.0/dist/leaflet.js"

data LMap = LMap T.Text | SetView (Double, Double) Double LMap

data LMapElement = TileLayer T.Text TileLayerProperties
                 | Marker (Double, Double)
                 | BindPopup T.Text LMapElement 

data TileLayerProperties = TileLayerProperties { attribution :: T.Text } deriving (Generic)
instance Aeson.ToJSON TileLayerProperties 

mapElementToJS :: LMapElement -> T.Text
mapElementToJS e' = "\n" <> f e' <> ".addTo(lmap);" where
 tshow = T.pack . show
 f (Marker (x, y)) = "L.marker(["<> tshow x<>", "<> tshow y<>"])"
 f (BindPopup t e) = f e <> ".bindPopup('"<>t<>"')"
 f (TileLayer url ps) = "L.tileLayer('"<>url<>"',"<>g ps<>")"
 g = T.decodeUtf8 . BSL.toStrict . Aeson.encode



osmTileLayer :: LMapElement 
osmTileLayer 
   = TileLayer "http://{s}.tile.osm.org/{z}/{x}/{y}.png" 
     $ TileLayerProperties "&copy; <a href=\"http://osm.org/copyright\">OpenStreetMap</a> contributors"

leafletCssCDN :: Monad m => HtmlT m ()
leafletCssCDN =
  link_ [rel_ "stylesheet",
         href_ "https://unpkg.com/leaflet@1.2.0/dist/leaflet.css"]

leafletMap ::  Monad m => LMap -> [LMapElement] -> HtmlT m ()
leafletMap mp elms = script_ $ writeMap mp <> writeElems elms where
  writeMap m = "\nvar lmap = " <> writeMap' m <> ";"
  writeMap' (LMap e) = "L.map('"<>e<>"')"
  writeMap' (SetView (x,y) z m) = writeMap' m <> ".setView(["<> tshow x<>", "<> tshow y<>"], "<> tshow z<>")"
  writeElems = T.unlines . map mapElementToJS 
  tshow = T.pack . show

