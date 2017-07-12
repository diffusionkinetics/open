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

import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Yaml
import           GHC.Generics               (Generic)
import           System.Directory           (getHomeDirectory)
import           System.FilePath            ((</>))

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

withConfigFile :: Maybe FilePath -> (DampfConfig -> IO ()) -> IO ()
withConfigFile mf action = loadConfigFile mf >>= action
{-# INLINE withConfigFile #-}


loadConfigFile :: Maybe FilePath -> IO DampfConfig
loadConfigFile (Just f) = parseConfig f
loadConfigFile _        = parseDefaultConfig
{-# INLINE loadConfigFile #-}


parseDefaultConfig :: IO DampfConfig
parseDefaultConfig = do
    cfgFile <- fmap (</> ".dampfcfg.yaml") getHomeDirectory
    parseConfig cfgFile
{-# INLINE parseDefaultConfig #-}


parseConfig :: FilePath -> IO DampfConfig
parseConfig f = decodeFile f >>= \case
    Just y  -> do
        ry <- subst defCfg <$> resolveEnvVars y
        parseMonad parseJSON ry

    Nothing -> error "Could not load config"
  where
    defCfg  = fromMaybe Null
        . parseMaybe parseJSON
        $ toJSON defaultConfig


subst :: Value -> Value -> Value
subst (Object x) (Object y) = Object $ HM.unionWith subst x y
subst _          y          = y

