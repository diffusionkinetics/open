{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  ) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Typeable

import Dampf.AppFile
import Dampf.ConfigFile


-- Dampf Exceptions

data DampfException
    = AppFileNotFound
    | ConfigFileNotFound
    deriving (Eq, Show, Typeable)


instance Exception DampfException


-- Dampf Context

data DampfContext = DampfContext
    { _app      :: Dampfs
    , _config   :: DampfConfig
    } deriving (Show)

makeLenses ''DampfContext


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


runDampfT :: Dampfs -> DampfConfig -> DampfT m a -> m a
runDampfT a c = flip runReaderT context . unDampfT
  where
    context = DampfContext a c

