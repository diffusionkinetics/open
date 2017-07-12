{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Dampf.ConfigFile
  ( -- * Configuration Types
    DampfConfig(..)
  , PostgresConfig(..)
    -- * Using Configurations
  , withConfigFile
  ) where

import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe               (fromMaybe)
import           Data.Yaml
import           GHC.Generics             (Generic)
import           System.Directory
import           System.FilePath


-- Configuration Types

data DampfConfig = DampfConfig
  { postgresPassword :: String
  , dbPasswords :: Map String String
  , liveCertificate :: Maybe FilePath

  , postgres :: PostgresConfig
  } deriving (Generic, Show)


instance FromJSON DampfConfig


data PostgresConfig = PostgresConfig
    { name  :: String
    , host  :: String
    , port  :: Int
    , users :: Map String String
    } deriving (Show, Generic)


instance FromJSON PostgresConfig


-- Default Configurations

defaultConfig :: DampfConfig
defaultConfig = DampfConfig
    { postgresPassword = ""
    , dbPasswords = Map.empty
    , liveCertificate = Nothing
    , postgres = defaultPostgresConfig
    }


defaultPostgresConfig :: PostgresConfig
defaultPostgresConfig = PostgresConfig
    { name  = "default"
    , host  = "localhost"
    , port  = 5432
    , users = Map.fromList [("postgres", "")]
    }


-- Using Configurations

withConfigFile :: Maybe FilePath -> (DampfConfig -> IO ()) -> IO ()
withConfigFile mfp thenDo = do
  home <- getHomeDirectory
  let fp = fromMaybe (home </>".dampfcfg.yaml") mfp
  ev <- decodeFileEither fp
  case ev of
    Right v -> thenDo v
    Left e -> fail $ show e
