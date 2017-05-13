{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mjml.Component where

import qualified Data.Text as T
import Lucid
import Lucid.Base

import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Blaze.ByteString.Builder (Builder)

import Lucid.Mjml.Unit

import Data.Monoid
import Control.Monad.State
import Control.Monad.Reader
import Data.Functor.Identity

import qualified Data.HashMap.Strict as HM

data MjmlState =
  MjmlState
  {
    mediaQueries :: HM.HashMap T.Text T.Text
  , fonts :: HM.HashMap T.Text T.Text
  }

emptyState :: MjmlState
emptyState = MjmlState HM.empty HM.empty

type MjmlT m = HtmlT (StateT MjmlState m)

data ElementContext =
  ElementContext
  {
    sibling :: Maybe Int
  , containerWidth :: Maybe T.Text
  }

data ElementT m a =
  ElementT
  {
    isRaw   :: Bool
  , renderer :: (MjmlT (ReaderT ElementContext m)) a
  }

addMediaQuery :: Monad m => T.Text -> WithUnit Int -> MjmlT m ()
addMediaQuery className pw = do
  st <- get
  let t = T.concat ["{ width:", T.pack (show pw) , "!important; }"]
      st' = st {mediaQueries = HM.insert className t (mediaQueries st)}

  put st'

generateStyles :: HM.HashMap T.Text T.Text -> T.Text
generateStyles = T.concat . HM.foldlWithKey' go []
  where
    go acc n v | v == T.empty = acc
               | otherwise = n : ":" : v : ";" : acc

build :: Monad m => Builder -> MjmlT m ()
build b = HtmlT (return (const b, ()))

-- | Folding and monoidally appending attributes.
foldlMapWithKey :: Monoid m => (k -> v -> m) -> HM.HashMap k v -> m
foldlMapWithKey f = HM.foldlWithKey' (\m k v -> m `mappend` f k v) mempty

-- | Convenience function for constructing builders.
s :: String -> Builder
s = Blaze.fromString
{-# INLINE s #-}

toPair :: Attribute -> (T.Text, T.Text)
toPair (Attribute x y) = (x,y)

lookupAttr :: T.Text -> HM.HashMap T.Text T.Text -> (T.Text, T.Text)
lookupAttr k m = (k, HM.lookupDefault T.empty k m)

-- conditionalTag :: 

-- renderMjml :: Monad m => [Element T m] -> 

border_ :: T.Text -> Attribute
border_ = makeAttribute "border"

cellpadding_ :: T.Text -> Attribute
cellpadding_ = makeAttribute "cellpadding"

cellspacing_ :: T.Text -> Attribute
cellspacing_ = makeAttribute "cellspacing"

backgroundColor_ :: T.Text -> Attribute
backgroundColor_ = makeAttribute "background-color"

background_ :: T.Text -> Attribute
background_ = makeAttribute "background"

align_ :: T.Text -> Attribute
align_ = makeAttribute "align"

