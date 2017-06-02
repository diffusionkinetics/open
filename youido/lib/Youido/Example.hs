{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   ExtendedDefaultRules, FlexibleContexts, TemplateHaskell,
   OverloadedLabels, TypeOperators, DataKinds #-}

module Youido.Example where

import Youido.Serve
import Youido.Types
import Youido.Dashdo
import Lucid
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Numeric.Datasets.Gapminder
import Numeric.Datasets
import Control.Monad.Reader
import Data.List (nub)
import Network.Wai
import Data.Text (Text, pack)
import Data.Monoid
import Lens.Micro.Platform
--import Graphics.Plotly.Lucid.hs

import Dashdo
import Dashdo.Types
import Dashdo.Elements

data Countries = Countries
               | Country Text

instance FromRequest Countries where
  fromRequest (rq,_) = case pathInfo rq of
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


data BubblesDD = BubblesDD { _selYear :: Int} deriving Show
makeLenses ''BubblesDD

bubblesDD gapM b = do
  let years = nub $ map year gapM
  select (map showOpt years) selYear
  h2_ "hello world"
  p_ (toHtml $ show $ _selYear b)


wrapIt :: Monad m => (a -> m (Html ())) -> (a -> m (Html ()))
wrapIt f x = do
  h <- f x
  return $ doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      cdnCSS
      cdnThemeCSS
      --plotlyCDN

    body_ $ do
      container_ h
      cdnJqueryJS
      cdnBootstrapJS
      script_ [src_ "/js/dashdo.js"] ""

runIt :: IO ()
runIt = do
  ddH <- dashdoGlobal
  gapM <- getDataset gapminder
  dd <- dashdoHandler #bubbles $ pureDashdo (BubblesDD 1980) (bubblesDD gapM)
  serve () [ ddH
           , H $ wrapIt dd
           , H $ wrapIt $ countryH gapM
           ]

