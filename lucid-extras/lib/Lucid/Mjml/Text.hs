{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mjml.Text where

import qualified Data.Text as T

import Data.Monoid(Monoid(..))
import Data.Semigroup(Semigroup(..))

import qualified Data.HashMap.Strict as M

import           Blaze.ByteString.Builder (Builder)

import Lucid.Base

import Lucid.Html5

import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze

startConditionalTag, endConditionalTag
  , startNegationConditionalTag, endNegationConditionalTag :: T.Text

startConditionalTag = "<!--[if mso | IE]>"
endConditionalTag = "<![endif]-->"

startNegationConditionalTag = "<!--[if !mso | IE]><!-->"
endNegationConditionalTag = "<!--<![endif]-->"

getTagsForNegation :: Bool -> (T.Text, T.Text)
getTagsForNegation True = (startConditionalTag, endConditionalTag)
getTagsForNegation False = (startNegationConditionalTag, endNegationConditionalTag)

conditionalTag :: Monad m => Bool -> HtmlT m () -> HtmlT m ()
conditionalTag negation = \m' ->
  HtmlT (do
            ~(f,a) <- runHtmlT m'
            return (const $ Blaze.fromText openTag <> f mempty <> Blaze.fromText closeTag, a))
  where
    (openTag, closeTag) = getTagsForNegation negation

border_, cellpadding_, cellspacing_ :: T.Text -> Attribute

border_ = makeAttribute "border"
cellpadding_ = makeAttribute "cellpadding"
cellspacing_ = makeAttribute "cellspacing"

insertConditionalTag :: Monad m => HtmlT m Builder -> HtmlT m a -> HtmlT m a
insertConditionalTag m' m'' =
  HtmlT (do
            ~(f, a) <- runHtmlT m'
            ~(f', a') <- runHtmlT m''
            return (\attr  -> Blaze.fromText startConditionalTag <> f attr <> Blaze.fromText endConditionalTag
                           <> f' mempty
                           <> Blaze.fromText startConditionalTag <> a <> Blaze.fromText endConditionalTag, a'))

-- makeElement' partially builds an element - it doesn't build the closing tag. It returns the
-- builder for the closing tag. This is done for more control when building elements for
-- use inside conditional tags
makeElement' :: Monad m
            => T.Text       -- ^ Name.
            -> HtmlT m Builder  -- ^ Children HTML.
            -> HtmlT m Builder -- ^ A parent element
makeElement' name m' = HtmlT (do
                                 ~(opTag, ()) <- runHtmlT (makeElementNoEnd name)
                                 ~(f,a) <- runHtmlT m'

                                 return (\attr -> (opTag attr) <> (f mempty),
                                         a <> Blaze.fromText "</" <> Blaze.fromText name <> Blaze.fromText ">"))

termWith' :: (Monad m, f ~ HtmlT m Builder) => T.Text -> [Attribute] -> f -> HtmlT m Builder
termWith' name = with (makeElement' name)

term' :: (Monad m, f ~ HtmlT m Builder) => T.Text -> f -> HtmlT m Builder
term' = flip termWith' []

defaultAttrs :: M.HashMap T.Text T.Text
defaultAttrs = M.fromList [("align", "left"), ("color", "#000000")]

-- | Build and encode an attribute.
buildAttr :: T.Text -> T.Text -> Builder
buildAttr key val =
  s " " <>
  Blaze.fromText key <>
  if val == mempty
     then mempty
     else s "=\"" <> Blaze.fromHtmlEscapedText val <> s "\""

attrsToStyle :: [Attribute] -> Attribute
attrsToStyle attrs = Blaze.

text_ :: (Monad m) => [Attribute] -> HtmlT m a -> HtmlT m a
text_ attr h = (insertConditionalTag (table__ [ role_ "presentation"
                     , border_ "0"
                     , cellpadding_ "0"
                     , cellspacing_ "0"
                     ] . tr__ . td__ [height_ height, style_ style] $ (return mempty))) $ (with div_ attr h)
  where
    table__ = termWith' "table"
    tr__ = term' "tr"
    td__ = termWith' "td"

    height = M.lookupDefault "height" "height" (M.fromListWith (<>) (map toPair attr))
    style=T.concat ["vertical-align:top;height:", height, "px;"]
    toPair (Attribute x y) = (x,y)

t1 :: Monad m => HtmlT m ()
t1 = text_ [height_ "2"] "Blah"

-- | Convenience function for constructing builders.
s :: String -> Builder
s = Blaze.fromString
{-# INLINE s #-}
