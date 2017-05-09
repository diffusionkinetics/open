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
    mediaQueries :: HM.HashMap T.Text Builder
  }

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
  let t = (Blaze.fromString "{ width:") <> Blaze.fromShow pw <> (Blaze.fromString "!important; }")
      st' = st {mediaQueries = HM.insert className t (mediaQueries st)}
  put st'

generateStyles :: HM.HashMap T.Text T.Text -> T.Text
generateStyles = T.concat . HM.foldlWithKey' (\acc n v -> n : ":" : v : acc ) []

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

border_ :: T.Text -> Attribute
border_ = makeAttribute "border"

cellpadding_ :: T.Text -> Attribute
cellpadding_ = makeAttribute "cellpadding"

cellspacing_ :: T.Text -> Attribute
cellspacing_ = makeAttribute "cellspacing"

