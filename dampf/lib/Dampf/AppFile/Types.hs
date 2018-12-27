{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns    #-}

module Dampf.AppFile.Types
  ( -- * Application Type
    DampfApp(..)
  , HasDampfApp(..)
    -- * Specification Types
  , ImageSpec(..)
  , HasImageSpec(..)
  , ContainerSpec(..)
  , HasContainerSpec(..)
  , DatabaseSpec(..)
  , HasDatabaseSpec(..)
  , DomainSpec(..)
  , HasDomainSpec(..)
  , FormData(..)
  , formAction
  , formContents
  , formMethod
  , Method(..)
  , TestSpec(..)
  , TestWhen(..)
  , TestUnit(..)
  , HasTestSpec(..)
  , Port (..)
  , traverseTestRunImageName
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import           Data.Map.Strict            (Map)
import           Data.Text                  (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Data.Char (toLower)
import           Data.Attoparsec.Text 
import           Data.Scientific

import           Dampf.Internal.Yaml

type ImageName = Text
type Command = Text
type URL = Text

data ImageSpec = ImageSpec
    { _dockerFile :: FilePath
    } deriving (Eq, Show, Generic)

makeClassy ''ImageSpec 

instance FromJSON ImageSpec where
    parseJSON = gDecode

data Port = Port Int | Map Int Int deriving (Eq, Generic)
instance Show Port 
  where show (Port p) = show p
        show (Map p p') = show p ++ ":" ++ show p

instance FromJSON Port where
  parseJSON (String s) = go s where
    go = either (const empty) pure . parseOnly parseMap
    parseMap = Map <$> (decimal <* char ':') <*> decimal
  
  parseJSON (Number n) = either (const empty) (pure . Port) . floatingOrInteger $ n
  parseJSON _ = empty

data ContainerSpec = ContainerSpec
    { _image        :: ImageName
    , _expose       :: Maybe [Port]
    , _command      :: Maybe Command
    , _useDatabase  :: Maybe Text
    } deriving (Eq, Show, Generic)

makeClassy ''ContainerSpec

instance FromJSON ContainerSpec where
    parseJSON = gDecode

data TestWhen = AtBuild | AtDeploy | Hourly | Daily | Frequently
    deriving (Show, Read, Eq, Ord, Generic)

data TestUnit = TestRun ImageName Command
              | TestGet URL (Maybe Text) -- match regex
    deriving (Eq, Show, Generic)

traverseTestRunImageName :: Applicative f => (Text -> f Text) -> TestUnit -> f TestUnit
traverseTestRunImageName inj (TestRun imgName cmd') = TestRun <$> inj imgName <*> pure cmd'
traverseTestRunImageName _   t = pure t

instance FromJSON TestWhen

data TestSpec = TestSpec
    { _tsUnits        :: [TestUnit]
    , _tsWhen         :: [TestWhen]
    , _tsForm         :: Maybe FormData
    } deriving (Eq, Show, Generic)

data Method = Post | Get
  deriving (Eq, Show)
instance FromJSON Method where
  parseJSON = withText "method" go
    where go "get"  = pure Get
          go "post" = pure Post
          go _ = mzero

type Action = String 
data FormData = FormData 
  { _formMethod :: Method
  , _formAction :: Action 
  , _formContents :: Map Text Text
  } deriving (Eq, Show)

makeClassy ''FormData

instance FromJSON FormData where
  parseJSON = withObject "form data" $ \o -> FormData 
    <$> o .: "method"
    <*> o .: "action"
    <*> o .: "content"

makeClassy ''TestSpec

instance FromJSON TestUnit where
    parseJSON = withText "test unit" $ \t->
        case T.words t of
            ("run":imgNm:rest) -> return $ TestRun imgNm (T.unwords rest)
            ("get":url:[]) -> return $ TestGet url Nothing
            ("get":url:"=~":regs) -> return $ TestGet url (Just $ T.unwords regs)
            _ -> fail $ "test unit parse fail: "++ T.unpack t

instance FromJSON TestSpec where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 . map toLower}

data DatabaseSpec = DatabaseSpec
    { _migrations   :: Maybe FilePath
    , _user         :: Text
    , _extensions   :: [Text]
    } deriving (Eq, Show, Generic)

makeClassy ''DatabaseSpec


instance FromJSON DatabaseSpec where
    parseJSON = gDecode


data DomainSpec = DomainSpec
    { _static           :: Maybe FilePath
    , _proxyContainer   :: Maybe Text
    , _letsEncrypt      :: Maybe Bool
    , _isCDN            :: Maybe Bool
    , _httpsOnly        :: Maybe Bool
    } deriving (Eq, Show, Generic)

makeClassy ''DomainSpec


instance FromJSON DomainSpec where
    parseJSON = gDecode


data DampfApp = DA
    { _images     :: Map Text ImageSpec
    , _containers :: Map Text ContainerSpec
    , _databases  :: Map Text DatabaseSpec
    , _domains    :: Map Text DomainSpec
    , _tests      :: Map Text TestSpec
    } deriving (Eq, Show, Generic)

makeClassy ''DampfApp


instance Monoid DampfApp where
    mempty = DA mempty mempty mempty mempty mempty


    mappend (DA a b c d e) (DA a' b' c' d' e') = DA
        (mappend a a') (mappend b b') (mappend c c') (mappend d d') (mappend e e')


instance FromJSON DampfApp where
    parseJSON = withObject "Application File" $ \m ->
        fmap mconcat $ forM (HM.toList m) $ \(k,v) ->
            case T.words k of
                ["image",     name] -> do
                    spec <- parseJSON v
                    return $ (images . at name ?~ spec) mempty

                ["container", name] -> do
                    spec <- parseJSON v
                    return $ (containers . at name ?~ spec) mempty

                ["postgresdb",  name] -> do
                    spec <- parseJSON v
                    return $ (databases . at name ?~ spec) mempty

                ["domain",    name] -> do
                    spec <- parseJSON v
                    return $ (domains . at name ?~ spec) mempty

                ["test",    name] -> do
                    spec <- parseJSON v
                    return $ (tests . at name ?~ spec) mempty

                _                   -> fail "Invalid specification type"

