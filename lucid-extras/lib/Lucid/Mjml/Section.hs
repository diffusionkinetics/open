{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mjml.Section
  (
    section_
  ) where

import Lucid.Mjml.Component
import Lucid.Mjml.Unit

import Lucid.Base
import Lucid.Html5 hiding (section_)

import Control.Monad.Reader

import Data.Monoid

import qualified Data.HashMap.Strict as HM

import qualified Data.Text as T

import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Blaze.ByteString.Builder (Builder)

import Lucid.Mjml.Attributes (align_, border_, cellpadding_, cellspacing_)

renderBefore :: T.Text -> Builder
renderBefore containerWidth = mempty

renderAfter :: Builder
renderAfter = mempty

render :: Monad m => HM.HashMap T.Text T.Text -> [ElementT m ()] -> MjmlT (ReaderT ElementContext m) ()
render attrs children = do
  ec <- ask
  let cw = case (containerWidth ec) of
        Nothing -> error "Needs containerWidth in context"
        Just t -> t

  build $ renderBefore cw
  div_ [style_ (generateStyles $ HM.fromList [("max-width", cw), ("margin", "0px auto")])] .
    table_ [align_ "center", border_ "0", cellpadding_ "0", cellspacing_ "0", role_ "presentation", style_ tableStyle] .
    tbody_ [] .
    tr_ . td_ [style_ tdStyle] $ forM_ children (\e -> local (\ec' ->  ec' {sibling = Just $ length children}) (renderer e))

  build renderAfter
  where
    fullAttrs = HM.union attrs $ HM.fromList [("direction", "ltr"), ("padding", "20px 0"), ("text-align", "center"), ("vertical-align", "top")]
    tableStyle = generateStyles $ HM.fromList [("font-size", "0px"), ("width", "100%")]
    tdStyle = generateStyles $ HM.fromList $
      [lookupAttr "direction" fullAttrs
      ,("font-size", "0px")
      ,lookupAttr "padding" fullAttrs
      ,lookupAttr "padding-bottom" fullAttrs
      ,lookupAttr "padding-right" fullAttrs
      ,lookupAttr "padding-top" fullAttrs
      ,lookupAttr "padding-left" fullAttrs
      ,("text-align", "center")
      ,("vertical-align", "top")]

section_ :: Monad m => [Attribute] -> [ElementT m ()] -> ElementT m ()
section_ attrs children = ElementT False (render attrMap children)
  where
     attrMap = HM.fromListWith (<>) (map toPair attrs)
