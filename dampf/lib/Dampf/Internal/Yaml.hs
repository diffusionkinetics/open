{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}

module Dampf.Internal.Yaml
  ( -- * Encoding and Decoding
    gDecode
    -- * Parsing
  , parseYaml
  ) where

import Data.Aeson.Types
import Data.Yaml
import GHC.Generics

import Dampf.Internal.Env


gDecode :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
gDecode = genericParseJSON options


options :: Options
options = defaultOptions
    { fieldLabelModifier = drop 1 }


parseYaml :: (FromJSON a) => FilePath -> IO a
parseYaml f = decodeFile f >>= \case
    Just y  -> resolveEnvVars y >>= parseMonad parseJSON
    Nothing -> error "Failed to parse YAML"

