{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}

module Dampf.Internal.Yaml
  ( gDecode
  , options
  , catchYamlException
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson.Types
import Data.Yaml
import GHC.Generics


gDecode :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
gDecode = genericParseJSON options


options :: Options
options = defaultOptions
    { fieldLabelModifier = drop 1
    , sumEncoding        = UntaggedValue
    }


catchYamlException :: (MonadIO m, MonadThrow m, Exception e)
    => (String -> e) -> ParseException -> m a
catchYamlException f e = throwM $ f e'
  where
    e' = prettyPrintParseException e

