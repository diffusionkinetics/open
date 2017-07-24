{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Dampf.Types
  ( -- * Dampf Exceptions
    DampfException(..)
    -- * Dampf Context
  , DampfContext
  , app
  , config
    -- * Dampf Monad Transformer
  , DampfT
  , runDampfT
    -- * Application Files
  , module AppFile
  , loadAppFile
    -- * Configuration Files
  , module ConfigFile
  , loadConfigFile
  ) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class                           (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Aeson.Types
import Data.Maybe                                       (fromMaybe)
import Data.Text                                        (Text)
import Data.Typeable
import Data.Yaml
import System.Directory
import System.FilePath

import Dampf.Internal.AppFile.Pretty as AppFile
import Dampf.Internal.AppFile.Types as AppFile
import Dampf.Internal.ConfigFile.Pretty as ConfigFile
import Dampf.Internal.ConfigFile.Types as ConfigFile
import Dampf.Internal.Env
import Dampf.Internal.Yaml


-- Dampf Exceptions

data DampfException
    = BadAppFile FilePath String
    | BadConfigFile FilePath String
    | NoDatabaseServer
    | NoMigrations
    | InvalidDatabase Text
    deriving (Eq, Show, Typeable)


instance Exception DampfException


-- Dampf Context

data DampfContext = DampfContext
    { _app      :: DampfApp
    , _config   :: DampfConfig
    } deriving (Show)

makeLenses ''DampfContext


instance HasDampfApp DampfContext where
    dampfApp = app


instance HasDampfConfig DampfContext where
    dampfConfig = config


-- Dampf Monad Transformer

newtype DampfT m a
    = DampfT { unDampfT :: ReaderT DampfContext m a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadReader DampfContext
        )


runDampfT :: DampfApp -> DampfConfig -> DampfT m a -> m a
runDampfT a c = flip runReaderT context . unDampfT
  where
    context = DampfContext a c


-- Loading YAML Files

loadAppFile :: (MonadIO m, MonadCatch m) => Maybe FilePath -> m DampfApp
loadAppFile mf = liftIO (decodeFile f >>= \case
    Just y  -> resolveEnvVars y >>= parseMonad parseJSON
    Nothing -> throwM $ BadAppFile f "") `catch`

    catchYamlException (BadAppFile f)
  where
    f = fromMaybe "dampf.yaml" mf


loadConfigFile :: (MonadIO m, MonadCatch m)
    => Maybe FilePath -> m (Either DampfProfiles DampfConfig)
loadConfigFile mf = do
    homeCfg <- liftIO $ fmap (</> ".dampfcfg.yaml") getHomeDirectory
    let f = fromMaybe homeCfg mf

    liftIO (decodeFile f >>= \case
        Just y  -> resolveEnvVars y >>= parseMonad (genericParseJSON options)
        Nothing -> throwM $ BadConfigFile f "") `catch`
        
        catchYamlException (BadConfigFile f)

