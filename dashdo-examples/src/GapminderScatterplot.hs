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

import Graphics.Plotly hiding (xaxis, yaxis)
import Graphics.Plotly.Lucid

data GmParams = GmParams
 {
   _selCountries :: [Text]
 , _selRegions  :: [Text]
 }

makeLenses ''GmParams

gm0 = GmParams [] []

countriesTable :: [Gapminder] -> SHtml GmParams ()
countriesTable gms =
  table_ [class_ "table table-striped"] $ do
    thead_ $ do
      tr_ $ do
        th_ "Country"
        th_ "Year"
        th_ "Population"
        th_ "Gdp per capita"
        th_ "Life expectancy"
    tbody_ $ do
      DF.for_ gms $ \(c) -> do
        tr_ $ do
          td_ $ toHtml (country          c)
          td_ $ toHtml (show $ year      c)
          td_ $ toHtml (show $ pop       c)
          td_ $ toHtml (show $ gdpPercap c)
          td_ $ toHtml (show $ lifeExp   c)

gmRenderer :: [Gapminder] -> GmParams -> () -> SHtml GmParams ()
gmRenderer gms gmParams () =
  wrap plotlyCDN $ do
    h1_ "Gapminder scatterplot example"
    row_ $ do
      mkCol [(MD,12)] $ do
        countriesTable gms

main = do
  gms <- getDataset gapminder
  runDashdo $ Dashdo gm0 (return . const ()) (gmRenderer gms)