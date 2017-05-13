{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Dampf.AppFile where

import GHC.Generics
import Data.Yaml
import Data.Aeson
import qualified Data.HashMap.Lazy as HM
import Control.Monad
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

data Dampf = Image T.Text ImageSpec
           | Domain T.Text DomainSpec
           | PostgresDB T.Text DBSpec
           | Container T.Text ContainerSpec

           deriving Show

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

data ContainerSpec = ContainerSpec
  { image :: T.Text } deriving (Generic, Show)

instance FromJSON ContainerSpec

data DomainSpec = DomainSpec
  { static :: Maybe T.Text
  , proxy_container :: Maybe T.Text } deriving (Generic, Show)

instance FromJSON DomainSpec

data DBSpec = DBSpec
  { migrations :: Maybe FilePath
  , db_user :: String
  , db_password :: String } deriving (Generic, Show)

instance FromJSON DBSpec

withAppFile :: Maybe FilePath -> (Dampfs -> IO ()) -> IO ()
withAppFile mfp thenDo = do
  let fp = fromMaybe "dampf.yaml" mfp
  ev <- decodeFileEither fp
  case ev of
    Right v -> thenDo v
    Left e -> fail $ show e