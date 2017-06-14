{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mjml.Button
  (
    button_
  ) where

import Lucid.Mjml.Component
import Lucid.Mjml.Unit

import Lucid.Base
import Lucid.Html5 hiding (button_)

import Control.Monad.Identity
import Control.Monad.Morph
import Control.Monad.Reader

import Data.Monoid
import Data.Maybe

import qualified Data.HashMap.Strict as HM

import qualified Data.Text as T

import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Blaze.ByteString.Builder (Builder)

import Lucid.Mjml.Attributes (align_, border_, cellpadding_, cellspacing_, bgcolor_, valign_)

defaultAttrs :: HM.HashMap T.Text T.Text
defaultAttrs = HM.fromList $
  [
    ("background-color", "#414141"),
    ("border", "none"),
    ("border-radius", "3px"),
    ("font-size", "13px"),
    ("font-weight", "normal"),
    ("font-family", "Ubuntu, Helvetica, Arial, sans-serif"),
    ("color", "#ffffff"),
    ("text-decoration", "none"),
    ("text-transform", "none"),
    ("align", "center"),
    ("vertical-align", "middle"),
    ("inner-padding", "10px 25px"),
    ("line-height", "120%"),
    ("padding", "10px 25px")
  ]

tdStyle, aStyle, tableStyle :: HM.HashMap T.Text T.Text -> T.Text
tdStyle attrs = generateStyles $ HM.fromList $ ("cursor", "auto") : ("padding", snd $ lookupAttr "inner-padding" attrs) : map (flip lookupAttr attrs)
      ["border"
      , "border-bottom"
      , "border-left"
      , "border-radius"
      , "border-right"
      , "border-top"
      , "color"
      , "font-style"
      , "height"]

aStyle attrs = generateStyles $ HM.fromList $ ("margin", "0px") : map (flip lookupAttr attrs)
  ["background-color"
  , "color"
  , "font-family"
  , "font-size"
  , "font-style"
  , "font-weight"
  , "line-height"
  , "text-decoration"
  , "text-transform"]

tableStyle attrs = generateStyles $ HM.fromList [lookupAttr "width" attrs]

renderButton :: Monad m => HM.HashMap T.Text T.Text -> HtmlT m () -> MjmlT m ()
renderButton attrs c = maybe withoutHref withHref $ HM.lookup "href" attrs
  where
    fullAttrs = HM.union attrs defaultAttrs
    withoutHref = p_ [style_ ""] (hoist lift c)
    withHref = \x -> a_ [href_ x, rel_ rel, style_ (aStyle fullAttrs), target_ "_blank"] (hoist lift c)
    rel = HM.lookupDefault "rel" "" attrs

render :: Monad m => HM.HashMap T.Text T.Text -> HtmlT m () -> MjmlT m ()
render attrs c = table_ [role_ "presentation"
                        , cellpadding_ "0"
                        , cellspacing_ "0"
                        , align_ align
                        , valign_ valign
                        , border_ "0"
                        , style_ $ tableStyle fullAttrs] $
                 tbody_ $ tr_ $ td_ [align_ "center", bgcolor_ bgcolor, style_ $ tdStyle fullAttrs
                                    , valign_ valign] (renderButton attrs c)
  where
    fullAttrs = HM.union attrs defaultAttrs
    align = snd $ lookupAttr "align" fullAttrs
    valign = snd $ lookupAttr "vertical-align" fullAttrs
    bgcolor = let t = snd $ lookupAttr "background-color" fullAttrs in
                if t == "none" then T.empty else t

button_ :: Monad m => [Attribute] -> Html () -> ElementT m ()
button_ attrs c = ElementT False (render attrMap (relaxHtmlT c))
  where
     attrMap = HM.fromListWith (<>) (map toPair attrs)
