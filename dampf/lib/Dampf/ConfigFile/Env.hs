{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Dampf.ConfigFile.Env
  ( resolveEnvVars
  ) where

import           Control.Monad              (forM)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text                  (Text)
import qualified Data.Text as T
import           Data.Vector                (Vector)
import           Data.Yaml
import           System.Environment


-- Resolving Environment Variables

resolveEnvVars :: Value -> IO Value
resolveEnvVars (Object m) = Object <$> resolveObject m
resolveEnvVars (Array vs) = Array  <$> resolveArray vs
resolveEnvVars (String t) = toJSON <$> resolveString t
resolveEnvVars x          = return x


resolveObject :: HashMap Text Value -> IO (HashMap Text Value)
resolveObject m = fmap HM.fromList . forM kvs $ \(k, v) -> do
    rk <- resolveString k
    rv <- resolveEnvVars v

    return (rk, rv)
  where
    kvs = HM.toList m


resolveArray :: Vector Value -> IO (Vector Value)
resolveArray = mapM resolveEnvVars


resolveString :: Text -> IO Text
resolveString t = case T.stripPrefix "$" t of
    Just e  -> let e' = T.unpack e in
        (lookup e' <$> getEnvironment) >>= \case
            Just v  -> return (T.pack v)
            Nothing -> error $ "Variable not in environment " ++ e'

    Nothing -> return t

