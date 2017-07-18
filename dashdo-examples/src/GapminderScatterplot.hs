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
import Data.List
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
 , _selContinents  :: [Text]
 , _selYear :: Text
 }

makeLenses ''GmParams

gm0 = GmParams [] [] "2007"

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
    trace =
      scatter
        & B.x ?~ (toJSON . gdpPercap <$> gms)
        & B.y ?~ (toJSON . lifeExp <$> gms)
        & B.customdata ?~ (toJSON . country <$> gms)
        & B.mode ?~ [B.Markers]
        & B.text ?~ (country <$> gms)
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

  in plotlySelectMultiple
    (plotly "countries-scatterplot" [trace]
      & layout %~ xaxis ?~ (defAxis 
          & axistype  ?~ Log
          & axistitle ?~ "GDP Per Capita"
          & tickvals ?~ (toJSON . fst <$> xTicks)
          & ticktext ?~ (pack . snd <$> xTicks))
      & layout %~ yaxis ?~ (defAxis & axistitle ?~ "Life Expectancy")
      & layout %~ title ?~ "GDP Per Capita and Life Expectancy") selCountries

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
      & labels ?~ pieLabels
      & values ?~ pieValues
      & customdata ?~ (toJSON <$> pieLabels)
      & hole ?~ (toJSON 0.4)
      & marker ?~ (defMarker
        & markercolors ?~ pieColors)

  in plotlySelectMultiple
    (plotly "continents-gdp" [trace]
      & layout %~ title ?~ "World's GDP by Continents") selContinents

average :: (Real a) => [a] -> Double
average xs = realToFrac (sum xs) / genericLength xs

yearsBarPlot :: [Gapminder] -> SHtml GmParams ()
yearsBarPlot gms =
  let
    years = nub $ year <$> gms
    trace = vbarChart [((pack . show) y, 
                        average (lifeExp <$> (filter ((==y) . year) gms))) 
                        | y <- years]
  in plotlySelect
    (plotly "years-lifeexp" [trace]
      & layout %~ title ?~ "Average Life Expactancy by Years") selYear

gmRenderer :: [Gapminder] -> GmParams -> () -> SHtml GmParams ()
gmRenderer gms gmParams () =
  wrap plotlyCDN $ do
    let selectedYear = read (unpack $ gmParams ^. selYear) :: Int
        
        yearFilter = filter ((==selectedYear) . year)

        countriesFilter = case gmParams ^. selCountries of
          [] -> id
          cs -> filter ((`elem` cs) . country)
        
        continentsFilter = case gmParams ^. selContinents of
          [] -> id
          cs -> filter ((`elem` cs) . continent)

        yearFilteredGMs = yearFilter gms
        
    h1_ "Gapminder Scatterplot Example"
    row_ $ do
      mkCol [(MD,4)] $ do
        continentsGDPsPie yearFilteredGMs
      mkCol [(MD,8)] $ do
        countriesScatterPlot $ continentsFilter yearFilteredGMs
    row_ $ do
      mkCol [(MD,12)] $ do
        yearsBarPlot $ (countriesFilter . continentsFilter) gms
    row_ $ do
      mkCol [(MD,12)] $ do
        countriesTable $ (countriesFilter . continentsFilter) yearFilteredGMs

main = do
  gms <- getDataset gapminder
  runDashdo $ Dashdo gm0 (return . const ()) (gmRenderer gms)