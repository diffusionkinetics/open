{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mjml.Container
  (
    container_
  ) where

import Lucid.Mjml.Component
import Lucid.Mjml.Unit

import Lucid.Base
import Lucid.Html5 hiding (section_)

import Control.Monad.Reader

import Data.Monoid

import qualified Data.HashMap.Strict as HM

import qualified Data.Text as T

render :: Monad m => HM.HashMap T.Text T.Text -> [ElementT m ()] -> MjmlT (ReaderT ElementContext m) ()
render attrs children = do
  div_ [style_ $ generateStyles fullAttrs, backgroundColor_ bgColor] $
    forM_ children (\e -> local (\ec' ->  ec' {containerWidth = Just cw}) (renderer e))

  where
    defaultAttrs = HM.fromList [("width", "600px")]
    fullAttrs = HM.union attrs defaultAttrs
    bgColor = snd $ lookupAttr "background-color" fullAttrs
    cw  = snd $ lookupAttr "width" fullAttrs

container_ :: Monad m => [Attribute] -> [ElementT m ()] -> ElementT m ()
container_ attrs children = ElementT False (render attrMap children)
  where
     attrMap = HM.fromListWith (<>) (map toPair attrs)
