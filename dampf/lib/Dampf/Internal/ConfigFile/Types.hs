{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Dampf.Internal.ConfigFile.Types
  ( -- * Configuration Types
    DampfConfig(..)
  , HasDampfConfig(..)
  , PostgresConfig(..)
  , HasPostgresConfig(..)
  ) where

import Control.Lens
import Data.Aeson.Types
import Data.Map.Strict  (Map)
import GHC.Generics     (Generic)

import Dampf.Internal.Yaml


-- Configuration Types

data PostgresConfig = PostgresConfig
    { _name  :: String
    , _host  :: String
    , _port  :: Int
    , _users :: Map String String
    } deriving (Show, Generic)

makeClassy ''PostgresConfig


instance FromJSON PostgresConfig where
    parseJSON = gDecode


data DampfConfig = DampfConfig
    { _liveCertificate :: Maybe FilePath
    , _postgres :: PostgresConfig
    } deriving (Show, Generic)

makeClassy ''DampfConfig


instance FromJSON DampfConfig where
    parseJSON = gDecode


instance HasPostgresConfig DampfConfig where
    postgresConfig = postgres

