{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Dampf.ConfigFile
  ( -- * Configuration Types
    DampfConfig(..)
  , PostgresConfig(..)
    -- * Using Configurations
  , loadConfigFile
  , withConfigFile
  ) where

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Yaml
import           GHC.Generics               (Generic)

import           Dampf.ConfigFile.Env


-- Configuration Types

data DampfConfig = DampfConfig
  { postgresPassword :: String
  , dbPasswords :: Map String String
  , liveCertificate :: Maybe FilePath

  , postgres :: PostgresConfig
  } deriving (Generic, Show)


instance ToJSON DampfConfig
instance FromJSON DampfConfig


data PostgresConfig = PostgresConfig
    { name  :: String
    , host  :: String
    , port  :: Int
    , users :: Map String String
    } deriving (Show, Generic)


instance ToJSON PostgresConfig
instance FromJSON PostgresConfig


-- Default Configurations

defaultConfig :: DampfConfig
defaultConfig = DampfConfig
    { postgresPassword  = ""
    , dbPasswords       = Map.empty
    , liveCertificate   = Nothing
    , postgres          = defaultPostgresConfig
    }


defaultPostgresConfig :: PostgresConfig
defaultPostgresConfig = PostgresConfig
    { name  = "default"
    , host  = "localhost"
    , port  = 5432
    , users = Map.fromList [("postgres", "")]
    }


-- Using Configurations

loadConfigFile :: Maybe FilePath -> IO DampfConfig
loadConfigFile mf = decodeFile cfgFile >>= \case
    Just y  -> do
        ry <- resolveEnvVars y
        parseMonad parseJSON ry

    Nothing -> error "Could not load config"
  where
    cfgFile = fromMaybe "~/.dampfcfg.yaml" mf


withConfigFile :: Maybe FilePath -> (DampfConfig -> IO ()) -> IO ()
withConfigFile mf action = loadConfigFile mf >>= action

