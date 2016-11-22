{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{-|

The classical Iris dataset, due to R.A. Fisher.

UCI ML Repository link <https://archive.ics.uci.edu/ml/datasets/Iris>

-}

module Numeric.Dataset.UCI.Wine where

import Numeric.Dataset.UCI

import Data.Csv
import GHC.Generics

data Wine = Wine
  { wineClass :: Int
  , alcohol :: Double
  , malicAcid :: Double
  , ash :: Double
  , ashAlcalinity :: Double
  , magnesium :: Double
  , totalPhenols :: Double
  , flavanoids :: Double
  , nonflavanoidPhenols :: Double
  , proanthocyanins :: Double
  , colorIntensity :: Double
  , hue :: Double
  , dilutedOD280toOD315 :: Double
  , proline :: Int
  } deriving (Show, Read, Generic)

instance FromRecord Wine

wine :: Dataset Wine
wine = csvDatasetPreprocess
            fixAmericanDecimals
            "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
