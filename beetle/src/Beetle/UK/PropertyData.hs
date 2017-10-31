{-# LANGUAGE DeriveGeneric, FlexibleInstances  #-}

module Beetle.UK.PropertyData where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Beetle.Base

type Postcode = String

data Prices = Prices Postcode Int
data Demographics = Demographics Postcode

instance ToURL (Key Prices) where
  toURL (Key k (Prices pc bedrms))
    = concat [ "http://api.propertydata.co.uk/prices?key=",
               k, "&postcode=",pc,"&bedrooms=", show bedrms]

data Wrapper a = Wrapper
  { wstatus :: String
  , wpoints_analysed :: Int
  , wdata :: a
  } deriving Generic

instance FromJSON a => FromJSON (Wrapper a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1 }

data PriceResp = PriceResp
  { average :: Double
  } deriving Generic

instance FromJSON PriceResp

getPrices :: Key Prices -> IO PriceResp
getPrices k = fmap wdata $ callAPI k