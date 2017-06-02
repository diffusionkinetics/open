{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   ExtendedDefaultRules, FlexibleContexts, TemplateHaskell,
   OverloadedLabels, TypeOperators, DataKinds #-}

module Youido.Example where

import Youido.Serve
import Youido.Types
import Youido.Dashdo
import Lucid
import Numeric.Datasets.Gapminder
import Numeric.Datasets
import Control.Monad.Reader
import Data.List (nub)
import Network.Wai
import Data.Text (Text, pack)
import Data.Monoid
import Lens.Micro.Platform

import Dashdo
import Dashdo.Types
import Dashdo.Elements

data BubblesDD = BubblesDD { _selYear :: Int}

makeLenses ''BubblesDD


runIt :: IO ()
runIt = do
  ddH <- dashdoGlobal
  gapM <- getDataset gapminder
  dd <- dashdoHandler $ pureDashdo (BubblesDD 1980) (bubblesDD gapM)
  serve () (ddH:H dd: hs gapM)

hs gapM =
     [ H (countryH gapM)
     ]

data Countries = Countries
               | Country Text

instance FromRequest Countries where
  fromRequest rq = case pathInfo rq of
    "countries":_ -> Just Countries
    "country":cnm:_ -> Just $ Country cnm
    _ -> Nothing

instance ToURL Countries where
  toURL Countries = "countries"
  toURL (Country cnm) = "country/"<>cnm

countryH :: [Gapminder] -> Countries -> ReaderT a IO (Html ())
countryH gapM Countries = do
  let countries = nub $ map country gapM
  return $ ul_ $ forM_ countries $ \c -> li_ $ a_ [href_ $ toURL $ Country c] $ (toHtml c)
countryH gapM (Country c) = do
  let entries = filter ((==c) . country) gapM
  return $ ul_ $ forM_ entries $ \e ->
    li_ $ "year: " <> toHtml (show $ year e) <> "  population: "<> toHtml (show $ pop e)


bubblesDD gapM b = do
  let years = nub $ map year gapM
  select (map showOpt years) selYear
  p_ "hello world"