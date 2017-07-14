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

import           Control.Lens
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict as Map
import           Data.Yaml
import           GHC.Generics               (Generic)


-- Configuration Types

data PostgresConfig = PostgresConfig
    { _name  :: String
    , _host  :: String
    , _port  :: Int
    , _users :: Map String String
    } deriving (Show, Generic)

makeClassy ''PostgresConfig


instance ToJSON PostgresConfig
instance FromJSON PostgresConfig where
    parseJSON (Object x) = PostgresConfig
        <$> x .:? "name"  .!= (defaultPostgres ^. name)
        <*> x .:? "host"  .!= (defaultPostgres ^. host)
        <*> x .:? "port"  .!= (defaultPostgres ^. port)
        <*> x .:? "users" .!= (defaultPostgres ^. users)

    parseJSON _          = error "Expecting Object"


defaultPostgres :: PostgresConfig
defaultPostgres = PostgresConfig
    { _name  = "default"
    , _host  = "localhost"
    , _port  = 5432
    , _users = Map.fromList [("postgres", "")]
    }


data DampfConfig = DampfConfig
    { _liveCertificate :: Maybe FilePath
    , _postgres :: PostgresConfig
    } deriving (Show, Generic)

makeClassy ''DampfConfig


instance ToJSON DampfConfig
instance FromJSON DampfConfig where
    parseJSON (Object x) = DampfConfig
        <$> x .:? "liveCertificate" .!= (defaultConfig ^. liveCertificate)
        <*> x .:? "postgres"        .!= (defaultConfig ^. postgres)

    parseJSON _          = error "Expecting Object"


instance HasPostgresConfig DampfConfig where
    postgresConfig = postgres


defaultConfig :: DampfConfig
defaultConfig = DampfConfig
    { _liveCertificate  = Nothing
    , _postgres         = defaultPostgres
    }

