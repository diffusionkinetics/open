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
import Lens.Micro.Platform
import Control.Monad

import Graphics.Plotly
import Graphics.Plotly.Lucid

data GmParams = GmParams
 {
   _selCountries :: [Text]
 , _selRegions  :: [Text]
 }

makeLenses ''GmParams

gm0 = GmParams [] []

data Continent = Africa | Americas | Asia | Europe | Oceania deriving (Eq, Show, Read)

continentRGB :: Continent -> RGB Int
continentRGB Africa   = RGB 255 165 0
continentRGB Americas = RGB 255 0 0
continentRGB Asia     = RGB 128 0 128
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
        th_ "Gdp per capita"
        th_ "Life expectancy"
    tbody_ $ do
      DF.for_ gms $ \(c) -> do
        tr_ $ do
          td_ $ toHtml (country          c)
          td_ $ toHtml (show $ pop       c)
          td_ $ toHtml (show $ gdpPercap c)
          td_ $ toHtml (show $ lifeExp   c)

countriesScatterPlot :: [Gapminder] -> SHtml GmParams ()
countriesScatterPlot gms =
  let
    trace :: Trace
    trace = points (aes & x .~ gdpPercap
                        & y .~ lifeExp
                        & color .~ Just (continentRGB . read . unpack . continent)) gms
  in toHtml $
    plotly "countries-scatterplot" [trace]
      & layout %~ xaxis .~ (Just $ 
        defAxis 
          & axistype  .~ Just Log
          & axistitle .~ Just "GDP Per Capita")
      & layout %~ yaxis .~ (Just $ defAxis & axistitle .~ Just "Life Expectancy")

gmRenderer :: [Gapminder] -> GmParams -> () -> SHtml GmParams ()
gmRenderer gms gmParams () =
  wrap plotlyCDN $ do
    h1_ "Gapminder scatterplot example"
    row_ $ do
      mkCol [(MD,8)] $ do
        countriesScatterPlot gms
      mkCol [(MD,4)] $ do
        "Doghunt chart goes here"
    row_ $ do
      mkCol [(MD,12)] $ do
        countriesTable gms

main = do
  gms <- getDataset gapminder
  runDashdo $ Dashdo gm0 (return . const ()) (gmRenderer $ filter ((== 2007) . year) gms)