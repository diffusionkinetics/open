{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Dampf.ConfigFile where

import GHC.Generics
import Data.Yaml
import Data.Maybe (fromMaybe)
import System.Directory
import System.FilePath
import Data.Map.Strict (Map)

data DampfConfig = DampfConfig
  { postgres_password :: String
  , db_passwords :: Map String String
  } deriving (Generic, Show)

instance FromJSON DampfConfig



withConfigFile :: Maybe FilePath -> (DampfConfig -> IO ()) -> IO ()
withConfigFile mfp thenDo = do
  home <- getHomeDirectory
  let fp = fromMaybe (home </>".dampfcfg.yaml") mfp
  ev <- decodeFileEither fp
  case ev of
    Right v -> thenDo v
    Left e -> fail $ show e