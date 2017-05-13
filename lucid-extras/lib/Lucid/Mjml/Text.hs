{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mjml.Text where


import Lucid.Mjml.Component

import qualified Data.Text as T

import Data.Semigroup(Semigroup(..))

import qualified Data.HashMap.Strict as HM

import Lucid.Base

import Lucid.Html5

-- startConditionalTag, endConditionalTag
--   , startNegationConditionalTag, endNegationConditionalTag :: T.Text

-- startConditionalTag = "<!--[if mso | IE]>"
-- endConditionalTag = "<![endif]-->"

-- startNegationConditionalTag = "<!--[if !mso | IE]><!-->"
-- endNegationConditionalTag = "<!--<![endif]-->"

-- getTagsForNegation :: Bool -> (T.Text, T.Text)
-- getTagsForNegation True = (startConditionalTag, endConditionalTag)
-- getTagsForNegation False = (startNegationConditionalTag, endNegationConditionalTag)

-- conditionalTag :: Monad m => Bool -> HtmlT m () -> HtmlT m ()
-- conditionalTag negation = \m' ->
--   HtmlT (do
--             ~(f,a) <- runHtmlT m'
--             return (const $ Blaze.fromText openTag <> f mempty <> Blaze.fromText closeTag, a))
--   where
--     (openTag, closeTag) = getTagsForNegation negation

-- border_, cellpadding_, cellspacing_ :: T.Text -> Attribute

-- border_ = makeAttribute "border"
-- cellpadding_ = makeAttribute "cellpadding"
-- cellspacing_ = makeAttribute "cellspacing"

-- insertConditionalTag :: Monad m => HtmlT m Builder -> HtmlT m a -> HtmlT m a
-- insertConditionalTag m' m'' =
--   HtmlT (do
--             ~(f, a) <- runHtmlT m'
--             ~(f', a') <- runHtmlT m''
--             return (\attr  -> Blaze.fromText startConditionalTag <> f attr <> Blaze.fromText endConditionalTag
--                            <> f' mempty
--                            <> Blaze.fromText startConditionalTag <> a <> Blaze.fromText endConditionalTag, a'))

-- -- makeElement' partially builds an element - it doesn't build the closing tag. It returns the
-- -- builder for the closing tag. This is done for more control when building elements for
-- -- use inside conditional tags
-- makeElement' :: Monad m
--             => T.Text       -- ^ Name.
--             -> HtmlT m Builder  -- ^ Children HTML.
--             -> HtmlT m Builder -- ^ A parent element
-- makeElement' name m' = HtmlT (do
--                                  ~(opTag, ()) <- runHtmlT (makeElementNoEnd name)
--                                  ~(f,a) <- runHtmlT m'

--                                  return (\attr -> (opTag attr) <> (f mempty),
--                                          a <> Blaze.fromText "</" <> Blaze.fromText name <> Blaze.fromText ">"))

-- termWith' :: (Monad m, f ~ HtmlT m Builder) => T.Text -> [Attribute] -> f -> HtmlT m Builder
-- termWith' name = with (makeElement' name)

-- term' :: (Monad m, f ~ HtmlT m Builder) => T.Text -> f -> HtmlT m Builder
-- term' = flip termWith' []

defaultAttrs :: HM.HashMap T.Text T.Text
defaultAttrs = HM.fromList [
  ("align", "left")
  , ("color", "#000000")
  , ("font-family", "Ubuntu, Helvetica, Arial, sans-serif")
  , ("font-size", "13px")
  , ("line-height", "1")
  , ("padding", "10px 25px")
  ]

render :: Monad m => HM.HashMap T.Text T.Text -> T.Text -> MjmlT m ()
render attrs content = div_ [style_ $ generateStyles style] (toHtml content)
  where
    fullAttrs = HM.union attrs defaultAttrs
    style = HM.fromList $ map (flip lookupAttr fullAttrs)
      ["font-family"
      , "font-size"
      , "font-weight"
      , "letter-spacing"
      , "line-height"
      , "text-align"
      , "text-decoration"
      , "text-transform"
      , "color"
      , "height"]

text_ :: Monad m => [Attribute] -> T.Text -> ElementT m ()
text_ attrs content = ElementT False (render (HM.fromListWith (<>) (map toPair attrs)) content)
