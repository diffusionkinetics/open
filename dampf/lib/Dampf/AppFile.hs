{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Dampf.AppFile where

import GHC.Generics
import Data.Yaml
import Data.Aeson
import qualified Data.HashMap.Lazy as HM
import Control.Monad
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

data Dampf = Image String ImageSpec
           | Domain String DomainSpec
           | PostgresDB String DBSpec
           | Container String ContainerSpec

           deriving Show

newtype Dampfs = Dampfs {unDampfs :: [Dampf] } deriving Show

instance FromJSON Dampfs where
  parseJSON = withObject "Dampf config" $ \hm -> do
    fmap Dampfs $ forM (HM.toList hm) $ \(k,v) -> do
      case T.words k of
        ["image", imname] -> do
          imspec <- parseJSON v
          return $ Image (T.unpack imname) imspec
        ["container", cname] ->
          Container <$> return (T.unpack cname) <*> parseJSON v
        ["postgresdb", dbname] ->
          PostgresDB <$> return (T.unpack dbname) <*> parseJSON v
        ["domain", dname] ->
          Domain <$> return (T.unpack dname) <*> parseJSON v
        _ -> fail $ "unknown dampf spec: "++ T.unpack k

data ImageSpec = ImageSpec
  { dockerFile :: FilePath } deriving (Generic, Show)

instance FromJSON ImageSpec

data ContainerSpec = ContainerSpec
  { image :: String,
    expose :: Maybe [Int],
    command :: Maybe String
  } deriving (Generic, Show)

instance FromJSON ContainerSpec

data DomainSpec = DomainSpec
  { static :: Maybe FilePath
  , proxy_container :: Maybe T.Text
  , letsencrypt :: Maybe Bool
  -- , generate :: Maybe T.Text
  } deriving (Generic, Show)

instance FromJSON DomainSpec

data DBSpec = DBSpec
  { migrations :: Maybe FilePath
  , db_user :: String
  , db_extensions :: [String] } deriving (Generic, Show)

instance FromJSON DBSpec

withAppFile :: Maybe FilePath -> (Dampfs -> IO ()) -> IO ()
withAppFile mfp thenDo = do
  let fp = fromMaybe "dampf.yaml" mfp
  ev <- decodeFileEither fp
  case ev of
    Right v -> thenDo v
    Left e -> fail $ show e