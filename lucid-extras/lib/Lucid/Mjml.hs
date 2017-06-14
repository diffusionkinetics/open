{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mjml (
  mjml_
  , renderMjmlT
  , module E) where

import Lucid.Mjml.Component as E
import Lucid.Mjml.Container as E
import Lucid.Mjml.Column as E
import Lucid.Mjml.Section as E
import Lucid.Mjml.Text as E
import Lucid.Mjml.Attributes as E
import Lucid.Mjml.Divider as E
import Lucid.Mjml.Button as E

import Lucid.Mjml.Skeleton
import Lucid.Base

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Morph


renderMjmlT :: Monad m => MjmlT m a -> HtmlT m a
renderMjmlT = hoist (flip evalStateT emptyState)

mjml_ :: Monad m => ElementT m () -> ElementT m () -> MjmlT m ()
mjml_ _ body = hoist (mapStateT (flip runReaderT emptyContext)) r
  where
    emptyContext = ElementContext Nothing Nothing
    r =  skeleton $ renderer body

