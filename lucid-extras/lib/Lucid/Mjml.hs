{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mjml where

import Lucid
import Lucid.Mjml.Component
import Lucid.Mjml.Skeleton

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

