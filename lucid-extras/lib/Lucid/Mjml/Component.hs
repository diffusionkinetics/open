{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lucid.Mjml.Component where

import qualified Data.Text as T
import Lucid
import Lucid.Base

import Control.Monad.State

import qualified Data.HashMap.Strict as HM

data MjmlState =
  MjmlState
  {

  }

type MjmlT m = HtmlT (StateT MjmlState m)

class Element a where
  isRaw :: a -> Bool
  render :: Monad m => a -> MjmlT m a

-- newtype MjmlT m a =
--   MjmlT
--   {
--     runMjmlT :: HtmlT (ReaderT [ComponentData] m) a
--   } deriving (Functor, Applicative, Monad)

-- generateStyles :: HM.HashMap T.Text T.Text -> T.Text
-- generateStyles = T.concat . HM.foldlWithKey' (\acc n v -> n : ":" : v : acc ) []

-- renderMjmlT :: Monad m => MjmlT m a -> HtmlT m a
-- renderMjmlT (MjmlT (HtmlT x)) = HtmlT (runReaderT x [])
