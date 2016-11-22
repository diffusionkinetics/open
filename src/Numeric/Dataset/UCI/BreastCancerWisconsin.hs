{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{-|

The classical Iris dataset, due to R.A. Fisher.

UCI ML Repository link <https://archive.ics.uci.edu/ml/datasets/Iris>

-}

module Numeric.Dataset.UCI.BreastCancerWisconsin where

import Numeric.Dataset.UCI

import Data.Csv
import GHC.Generics
import Control.Applicative


data Diagnosis = Malignant | Benign deriving (Show, Read, Eq, Generic)

intToDiagnosis :: Int -> Diagnosis
intToDiagnosis 2 = Benign
intToDiagnosis 4 = Malignant
intToDiagnosis _ = error "unknown diagnosis code"

data BreastCancerEntry = BreastCancerEntry
 { sampleCodeNumber :: Int
 , clumpThickness :: Int
 , uniformityCellSize :: Int
 , uniformityCellShape :: Int
 , marginalAdhesion :: Int
 , singleEpithelialCellSize :: Int
 , bareNuclei :: Maybe Int
 , blandChromatin :: Int
 , normalNucleoli :: Int
 , mitosis :: Int
 , sampleClass :: Diagnosis
 } deriving (Show, Read, Generic)

instance FromRecord BreastCancerEntry where
  parseRecord v = BreastCancerEntry <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> (v .! 6 <|> return Nothing) <*> v .! 7  <*> v .! 8  <*> v .! 9  <*> (intToDiagnosis <$> v .! 10)

breastCancerDatabase :: Dataset BreastCancerEntry
breastCancerDatabase = csvDataset
   "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
