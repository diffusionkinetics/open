{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mjml
  (
    module Lucid.Mjml
  , module H) where

import Data.Text

import Lucid (Term(..), Attribute)
import Lucid.Base (makeAttribute)

import Lucid.Html5 as H (src_, width_)

-------------------------------------------------------------------------------
-- Standard Components

-- | @mjml@ component
mjml_ :: Term arg result => arg -> result
mjml_ = term "mjml"

-- | @mj-body@ component
body_ :: Term arg result => arg -> result
body_ = term "mj-body"

-- | @mj-container@ component
container_ :: Term arg result => arg -> result
container_ = term "mj-container"

-- | @mj-section@ component
section_ :: Term arg result => arg -> result
section_ = term "mj-section"

-- | @mj-column@ component
column_ :: Term arg result => arg -> result
column_ = term "mj-column"

-- | @mj-image@ component
image_ :: Term arg result => arg -> result
image_ = term "mj-image"

-- | @mj-divier@ component
divider_ :: Term arg result => arg -> result
divider_ = term "mj-divier"

-- | @mj-text@ component
text_ :: Term arg result => arg -> result
text_ = term "mj-text"


-------------------------------------------------------------------------------
-- Attributes

-- | The @border-color@ attribute.
borderColor_ :: Text -> Attribute
borderColor_ = makeAttribute "border-color"

-- | The @font-size@ attribute.
fontSize_ :: Text -> Attribute
fontSize_ = makeAttribute "font-size"

-- | The @font-family@ attribute.
fontFamily_ :: Text -> Attribute
fontFamily_ = makeAttribute "font-family"

-- | The @color@ attribute.
color_ :: Text -> Attribute
color_ = makeAttribute "color"

-- | The @color@ attribute.
backgroundColor_ :: Text -> Attribute
backgroudColor_ = makeAttribute "color"

