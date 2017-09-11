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
import Dashdo.FlexibleInput

data Countries = Countries
               | Country Text

instance FromRequest Countries where
  fromRequest (rq,_) = case pathInfo rq of
    "countries":_ -> Just Countries
    "country":cnm:_ -> Just $ Country cnm
    _ -> Nothing

instance ToURL Countries where
  toURL Countries = "/countries"
  toURL (Country cnm) = "/country/"<>cnm

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

bubblesDD gapM = do
  let years = nub $ map year gapM
  selYear <<~ select (map showOpt years)
  h2_ "hello world"
  BubblesDD y <- getValue
  p_ (toHtml $ show $ y)



sidebar = mkSidebar
    [ "Bubbles"  *~ #bubbles :/ Initial
    , "Counties" *~ Countries
    ]

runIt :: IO ()
runIt = do
  ddH <- dashdoGlobal
  gapM <- getDataset gapminder
  dd <- flip runReaderT () $ dashdoHandler #bubbles $ Dashdo (BubblesDD 1980) (bubblesDD gapM)
  serve () $ Youido
              [ ddH  -- handler for /uuid and public/js
              , H dd -- handler for /bubbles
              , H $ countryH gapM -- handler for /countries
              ]
              "Not found!"  -- if nothing found
              (stdWrapper (mempty) sidebar)  -- wrapper for html
              []  -- basic auth users
              3101  -- port
