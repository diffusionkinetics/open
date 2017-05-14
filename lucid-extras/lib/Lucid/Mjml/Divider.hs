{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mjml.Divider
  (
    divider_
  ) where

import Lucid.Mjml.Component

import Lucid.Base
import Lucid.Html5

import Data.Monoid

import qualified Data.HashMap.Strict as HM

import qualified Data.Text as T

render :: Monad m => HM.HashMap T.Text T.Text ->  MjmlT m ()
render attrs = rendererWrapper fullAttrs $ p_ [style_ $ generateStyles style] (return ())
  where
    defaultAttrs = HM.fromList [("border-color", "#000000"), ("border-style", "solid")
                               , ("border-width", "4px"), ("padding", "10px 25px")
                               , ("width", "100%")]
    fullAttrs = HM.union attrs defaultAttrs
    [bw, bs, bc] = snd . flip lookupAttr fullAttrs <$> ["border-width", "border-style", "border-color"]
    style = HM.fromList
      [("border-top", T.unwords [bw, bs, bc])
      , ("font-size", "1px")
      , ("margin", "0px auto")
      , lookupAttr "width" fullAttrs
      ]

divider_ :: Monad m => [Attribute] -> ElementT m ()
divider_ attrs = ElementT False (render attrMap)
  where
     attrMap = HM.fromListWith (<>) (map toPair attrs)
