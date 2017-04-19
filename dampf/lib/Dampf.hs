{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Dampf where

import GHC.Generics
import Data.Yaml
import Data.Aeson
import qualified Data.HashMap.Lazy as HM
import Control.Monad
import qualified Data.Text as T

data Dampf = Image T.Text ImageSpec  deriving Show

newtype Dampfs = Dampfs {unDampfs :: [Dampf] } deriving Show

instance FromJSON Dampfs where
  parseJSON = withObject "Dampf config" $ \hm -> do
    fmap Dampfs $ forM (HM.toList hm) $ \(k,v) -> do
      case T.words k of 
        ["image", imname] -> do 
          imspec <- parseJSON v
          return $ Image imname imspec
        _ -> fail $ "unknown dampf spec: "++ T.unpack k

data ImageSpec = ImageSpec
  { dockerFile :: FilePath } deriving (Generic, Show)

instance FromJSON ImageSpec

dumpYaml :: FilePath -> IO ()
dumpYaml fp = do
  Just v <- decodeFile fp
  print (v::Value)

dumpCfg :: FilePath -> IO ()
dumpCfg fp = do
  ev <- decodeFileEither fp
  case ev of
    Right v -> print (v::Dampfs)
    Left e -> fail $ show e
