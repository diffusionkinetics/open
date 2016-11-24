{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{-|

The classical Iris dataset, due to R.A. Fisher.

UCI ML Repository link <https://archive.ics.uci.edu/ml/datasets/Iris>

-}

module Numeric.Datasets.Abalone where

import Numeric.Datasets

import Data.Csv
import GHC.Generics

data Sex = M | F | I
  deriving (Show, Read, Eq, Generic)

instance FromField Sex where
  parseField = parseReadField

data Abalone = Abalone
  { sex :: Sex
  , abaloneLength :: Double
  , diameter :: Double
  , height :: Double
  , wholeWeight :: Double
  , shuckedWeight :: Double
  , visceraWeight :: Double
  , shellWeight :: Double
  , rings :: Int
  } deriving (Show, Read, Generic)

instance FromRecord Abalone

abalone :: Dataset Abalone
abalone = csvDataset "http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
