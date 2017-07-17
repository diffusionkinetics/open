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


-- Configuration Types

data PostgresConfig = PostgresConfig
    { _name  :: String
    , _host  :: String
    , _port  :: Int
    , _users :: Map String String
    } deriving (Show, Generic)

makeClassy ''PostgresConfig


instance ToJSON PostgresConfig where
    toJSON = genericToJSON opts
      where
        opts = defaultOptions { fieldLabelModifier = drop 1 }


instance FromJSON PostgresConfig where
    parseJSON = genericParseJSON opts
      where
        opts = defaultOptions { fieldLabelModifier = drop 1 }


data DampfConfig = DampfConfig
    { _liveCertificate :: Maybe FilePath
    , _postgres :: PostgresConfig
    } deriving (Show, Generic)

makeClassy ''DampfConfig


instance ToJSON DampfConfig where
    toJSON = genericToJSON opts
      where
        opts = defaultOptions { fieldLabelModifier = drop 1 }


instance FromJSON DampfConfig where
    parseJSON = genericParseJSON opts
      where
        opts = defaultOptions { fieldLabelModifier = drop 1 }


instance HasPostgresConfig DampfConfig where
    postgresConfig = postgres

