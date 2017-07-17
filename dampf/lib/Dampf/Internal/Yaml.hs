{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}

module Dampf.Internal.Yaml
  ( -- * Encoding and Decoding
    gEncode
  , gDecode
    -- * Parsing
  , parseYaml
  ) where

import Data.Aeson.Types
import Data.Yaml
import GHC.Generics

import Dampf.Internal.Env


gEncode :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
gEncode = genericToJSON options


gDecode :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
gDecode = genericParseJSON options


options :: Options
options = defaultOptions
    { fieldLabelModifier = drop 1 }


parseYaml :: (FromJSON a) => FilePath -> IO a
parseYaml f = decodeFile f >>= \case
    Just y  -> resolveEnvVars y >>= parseMonad parseJSON
    Nothing -> error "Failed to parse YAML"

