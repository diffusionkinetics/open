{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, TemplateHaskell #-}

import Numeric.Datasets
import Numeric.Datasets.Gapminder

import Dashdo
import Dashdo.Types
import Dashdo.Serve
import Dashdo.Elements
import Lucid
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Data.Monoid ((<>))
import qualified Data.Foldable as DF
import Data.Text (Text, unpack, pack)
import Data.Aeson.Types
import GHC.Float
import Lens.Micro.Platform
import Control.Monad
import Control.Applicative

import Graphics.Plotly
import qualified Graphics.Plotly.Base as B
import Graphics.Plotly.Lucid

data GmParams = GmParams
 {
   _selCountries :: [Text]
 , _selRegions  :: [Text]
 }

makeLenses ''GmParams

gm0 = GmParams [] []

data Continent = Africa | Americas | Asia | Europe | Oceania deriving (Eq, Show, Read, Enum)

continentRGB :: Continent -> RGB Int
continentRGB Africa   = RGB 255 165 0
continentRGB Americas = RGB 178 34 34
continentRGB Asia     = RGB 218 112 214
continentRGB Europe   = RGB 0 128 0
continentRGB Oceania  = RGB 0 0 255
continentRGB _ = undefined

-- TODO: 1 - doghunt 2- sum gdp for region
-- TODO: plotlySelectMultiple

countriesTable :: [Gapminder] -> SHtml GmParams ()
countriesTable gms =
  table_ [class_ "table table-striped"] $ do
    thead_ $ do
      tr_ $ do
        th_ "Country"
        th_ "Population"
        th_ "GDP per capita"
        th_ "GDP"
        th_ "Life expectancy"
    tbody_ $ do
      DF.for_ gms $ \(c) -> do
        tr_ $ do
          td_ $ toHtml (country          c)
          td_ $ toHtml (show $ pop       c)
          td_ $ toHtml (show $ gdpPercap c)
          td_ $ toHtml (show $ gdp       c)
          td_ $ toHtml (show $ lifeExp   c)

countriesScatterPlot :: [Gapminder] -> SHtml GmParams ()
countriesScatterPlot gms =
  let
    trace :: Trace
    trace =
      scatter
        & B.x ?~ (toJSON . gdpPercap <$> gms)
        & B.y ?~ (toJSON . lifeExp <$> gms)
        & B.mode ?~ [B.Markers]
        & B.marker ?~ 
          (defMarker
            & B.markercolor ?~ (List $ toJSON . continentRGB . read . unpack . continent <$> gms)
            & B.size ?~ (List $ (toJSON . float2Double . (* 200000) . log . fromInteger . pop) <$> gms)
            & B.sizeref ?~ toJSON 2e5
            & B.sizeMode ?~ Area)

    xTicks = [
        (1000,  "$ 1,000")
      , (10000, "$ 10,000")
      , (50000, "$ 50,000")]

  in toHtml $
    plotly "countries-scatterplot" [trace]
      & layout %~ xaxis .~ (Just $ 
        defAxis 
          & axistype  ?~ Log
          & axistitle ?~ "GDP Per Capita"
          & tickvals ?~ (toJSON . fst <$> xTicks)
          & ticktext ?~ (pack . snd <$> xTicks))
      & layout %~ yaxis .~ (Just $ defAxis & axistitle .~ Just "Life Expectancy")
      & layout %~ title .~ Just "GDP Per Capita and Life Exmectancy"

gdp :: Gapminder -> Double
gdp c = gdpPercap c * (fromIntegral $ pop c)

continentsGDPsPie :: [Gapminder] -> SHtml GmParams ()
continentsGDPsPie gms = 
  let
    -- list of continent-GDP pairs [(Continent, Double)]
    -- for each continent from Africa to Oceania
    continentGdps = 
      [((,) currentCont) . sum $ 
        gdp <$>
          (filter ((== show currentCont) . unpack . continent) gms) 
          | currentCont <- [ Africa .. Oceania ]]
    
    pieLabels = pack . show . fst <$> continentGdps
    pieValues = toJSON . snd <$> continentGdps
    pieColors = List $ (toJSON . continentRGB . fst) <$> continentGdps

    trace :: Trace
    trace = pie
      & labels .~ Just pieLabels
      & values .~ Just pieValues
      & hole .~ Just (toJSON 0.4)
      & marker .~ (Just $
        defMarker
          & markercolors .~ Just pieColors)

  in toHtml $ 
    plotly "continents-gdp" [trace]
      & layout %~ title .~ Just "World's GDP by continents"

gmRenderer :: [Gapminder] -> GmParams -> () -> SHtml GmParams ()
gmRenderer gms gmParams () =
  wrap plotlyCDN $ do
    h1_ "Gapminder scatterplot example"
    row_ $ do
      mkCol [(MD,8)] $ do
        countriesScatterPlot gms
      mkCol [(MD,4)] $ do
        continentsGDPsPie gms
    row_ $ do
      mkCol [(MD,12)] $ do
        countriesTable gms

main = do
  gms <- getDataset gapminder
  runDashdo $ Dashdo gm0 (return . const ()) (gmRenderer $ filter ((== 2007) . year) gms)