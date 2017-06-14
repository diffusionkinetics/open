{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mjml.Attributes where

import Lucid.Base
import qualified Data.Text as T

-- | @align@ attribute
align_ :: T.Text -> Attribute
align_ = makeAttribute "align"

-- | @background-color@ attribute
backgroundColor_ :: T.Text -> Attribute
backgroundColor_ = makeAttribute "background-color"

-- | @background-height@ attribute
backgroundHeight_ :: T.Text -> Attribute
backgroundHeight_ = makeAttribute "background-height"

-- | @background-position@ attribute
backgroundPosition_ :: T.Text -> Attribute
backgroundPosition_ = makeAttribute "background-position"

-- | @background-repeat@ attribute
backgroundRepeat_ :: T.Text -> Attribute
backgroundRepeat_ = makeAttribute "background-repeat"

-- | @background-size@ attribute
backgroundSize_ :: T.Text -> Attribute
backgroundSize_ = makeAttribute "background-size"

-- | @background-url@ attribute
backgroundUrl_ :: T.Text -> Attribute
backgroundUrl_ = makeAttribute "background-url"

-- | @background-width@ attribute
backgroundWidth_ :: T.Text -> Attribute
backgroundWidth_ = makeAttribute "background-width"

-- | @background@ attribute
background_ :: T.Text -> Attribute
background_ = makeAttribute "background"

-- | @base url@ attribute
baseUrl_ :: T.Text -> Attribute
baseUrl_ = makeAttribute "base url"

-- | @border@ attribute
border_ :: T.Text -> Attribute
border_ = makeAttribute "border"

-- | @border-bottom@ attribute
borderBottom_ :: T.Text -> Attribute
borderBottom_ = makeAttribute "border-bottom"

-- | @border-color@ attribute
borderColor_ :: T.Text -> Attribute
borderColor_ = makeAttribute "border-color"

-- | @border-left@ attribute
borderLeft_ :: T.Text -> Attribute
borderLeft_ = makeAttribute "border-left"

-- | @border-radius@ attribute
borderRadius_ :: T.Text -> Attribute
borderRadius_ = makeAttribute "border-radius"

-- | @border-right@ attribute
borderRight_ :: T.Text -> Attribute
borderRight_ = makeAttribute "border-right"

-- | @border-style@ attribute
borderStyle_ :: T.Text -> Attribute
borderStyle_ = makeAttribute "border-style"

-- | @border-top@ attribute
borderTop_ :: T.Text -> Attribute
borderTop_ = makeAttribute "border-top"

-- | @border-width@ attribute
borderWidth_ :: T.Text -> Attribute
borderWidth_ = makeAttribute "border-width"

-- | @cellpadding@ attribute
cellpadding_ :: T.Text -> Attribute
cellpadding_ = makeAttribute "cellpadding"

-- | @cellspacing@ attribute
cellspacing_ :: T.Text -> Attribute
cellspacing_ = makeAttribute "cellspacing"

-- | @color@ attribute
color_ :: T.Text -> Attribute
color_ = makeAttribute "color"

-- | @container-background-color@ attribute
containerBackgroundColor_ :: T.Text -> Attribute
containerBackgroundColor_ = makeAttribute "container-background-color"

-- | @direction@ attribute
direction_ :: T.Text -> Attribute
direction_ = makeAttribute "direction"

-- | @display@ attribute
display_ :: T.Text -> Attribute
display_ = makeAttribute "display"

-- | @facebook-content@ attribute
facebookContent_ :: T.Text -> Attribute
facebookContent_ = makeAttribute "facebook-content"

-- | @facebook-href@ attribute
facebookHref_ :: T.Text -> Attribute
facebookHref_ = makeAttribute "facebook-href"

-- | @facebook-icon-color@ attribute
facebookIconColor_ :: T.Text -> Attribute
facebookIconColor_ = makeAttribute "facebook-icon-color"

-- | @font-family@ attribute
fontFamily_ :: T.Text -> Attribute
fontFamily_ = makeAttribute "font-family"

-- | @font-size@ attribute
fontSize_ :: T.Text -> Attribute
fontSize_ = makeAttribute "font-size"

-- | @font-style@ attribute
fontStyle_ :: T.Text -> Attribute
fontStyle_ = makeAttribute "font-style"

-- | @font-weight@ attribute
fontWeight_ :: T.Text -> Attribute
fontWeight_ = makeAttribute "font-weight"

-- | @format@ attribute
format_ :: T.Text -> Attribute
format_ = makeAttribute "format"

-- | @full-width@ attribute
fullWidth_ :: T.Text -> Attribute
fullWidth_ = makeAttribute "full-width"

-- | @googleham-content@ attribute
googlehamContent_ :: T.Text -> Attribute
googlehamContent_ = makeAttribute "googleham-content"

-- | @google-href@ attribute
googleHref_ :: T.Text -> Attribute
googleHref_ = makeAttribute "google-href"

-- | @google-icon-color@ attribute
googleIconColor_ :: T.Text -> Attribute
googleIconColor_ = makeAttribute "google-icon-color"

-- | @hamburger@ attribute
hamburger_ :: T.Text -> Attribute
hamburger_ = makeAttribute "hamburger"

-- | @ico-align@ attribute
icoAlign_ :: T.Text -> Attribute
icoAlign_ = makeAttribute "ico-align"

-- | @ico-close@ attribute
icoClose_ :: T.Text -> Attribute
icoClose_ = makeAttribute "ico-close"

-- | @ico-color@ attribute
icoColor_ :: T.Text -> Attribute
icoColor_ = makeAttribute "ico-color"

-- | @ico-font-family@ attribute
icoFontFamily_ :: T.Text -> Attribute
icoFontFamily_ = makeAttribute "ico-font-family"

-- | @ico-font-size@ attribute
icoFontSize_ :: T.Text -> Attribute
icoFontSize_ = makeAttribute "ico-font-size"

-- | @ico-line-height@ attribute
icoLineHeight_ :: T.Text -> Attribute
icoLineHeight_ = makeAttribute "ico-line-height"

-- | @ico-open@ attribute
icoOpen_ :: T.Text -> Attribute
icoOpen_ = makeAttribute "ico-open"

-- | @ico-padding@ attribute
icoPadding_ :: T.Text -> Attribute
icoPadding_ = makeAttribute "ico-padding"

-- | @ico-padding-bottom@ attribute
icoPaddingBottom_ :: T.Text -> Attribute
icoPaddingBottom_ = makeAttribute "ico-padding-bottom"

-- | @ico-padding-left@ attribute
icoPaddingLeft_ :: T.Text -> Attribute
icoPaddingLeft_ = makeAttribute "ico-padding-left"

-- | @ico-padding-right@ attribute
icoPaddingRight_ :: T.Text -> Attribute
icoPaddingRight_ = makeAttribute "ico-padding-right"

-- | @ico-padding-top@ attribute
icoPaddingTop_ :: T.Text -> Attribute
icoPaddingTop_ = makeAttribute "ico-padding-top"

-- | @ico-text-decoration@ attribute
icoTextDecoration_ :: T.Text -> Attribute
icoTextDecoration_ = makeAttribute "ico-text-decoration"

-- | @ico-text-transform@ attribute
icoTextTransform_ :: T.Text -> Attribute
icoTextTransform_ = makeAttribute "ico-text-transform"

-- | @icon-align@ attribute
iconAlign_ :: T.Text -> Attribute
iconAlign_ = makeAttribute "icon-align"

-- | @icon-height@ attribute
iconHeight_ :: T.Text -> Attribute
iconHeight_ = makeAttribute "icon-height"

-- | @icon-position@ attribute
iconPosition_ :: T.Text -> Attribute
iconPosition_ = makeAttribute "icon-position"

-- | @icon-size@ attribute
iconSize_ :: T.Text -> Attribute
iconSize_ = makeAttribute "icon-size"

-- | @icon-unwrapped-alt@ attribute
iconUnwrappedAlt_ :: T.Text -> Attribute
iconUnwrappedAlt_ = makeAttribute "icon-unwrapped-alt"

-- | @icon-unwrapped-url@ attribute
iconUnwrappedUrl_ :: T.Text -> Attribute
iconUnwrappedUrl_ = makeAttribute "icon-unwrapped-url"

-- | @icon-width@ attribute
iconWidth_ :: T.Text -> Attribute
iconWidth_ = makeAttribute "icon-width"

-- | @icon-wrapped-alt@ attribute
iconWrappedAlt_ :: T.Text -> Attribute
iconWrappedAlt_ = makeAttribute "icon-wrapped-alt"

-- | @icon-wrapped-url@ attribute
iconWrappedUrl_ :: T.Text -> Attribute
iconWrappedUrl_ = makeAttribute "icon-wrapped-url"

-- | @img-src@ attribute
imgSrc_ :: T.Text -> Attribute
imgSrc_ = makeAttribute "img-src"

-- | @inline@ attribute
inline_ :: T.Text -> Attribute
inline_ = makeAttribute "inline"

-- | @inner-padding@ attribute
innerPadding_ :: T.Text -> Attribute
innerPadding_ = makeAttribute "inner-padding"

-- | @instagram-content@ attribute
instagramContent_ :: T.Text -> Attribute
instagramContent_ = makeAttribute "instagram-content"

-- | @instagram-href@ attribute
instagramHref_ :: T.Text -> Attribute
instagramHref_ = makeAttribute "instagram-href"

-- | @instagram-icon-color@ attribute
instagramIconColor_ :: T.Text -> Attribute
instagramIconColor_ = makeAttribute "instagram-icon-color"

-- | @intl@ attribute
intl_ :: T.Text -> Attribute
intl_ = makeAttribute "intl"

-- | @left-icon@ attribute
leftIcon_ :: T.Text -> Attribute
leftIcon_ = makeAttribute "left-icon"

-- | @letter-spacing@ attribute
letterSpacing_ :: T.Text -> Attribute
letterSpacing_ = makeAttribute "letter-spacing"

-- | @line-height@ attribute
lineHeight_ :: T.Text -> Attribute
lineHeight_ = makeAttribute "line-height"

-- | @linkedin-content@ attribute
linkedinContent_ :: T.Text -> Attribute
linkedinContent_ = makeAttribute "linkedin-content"

-- | @linkedin-href@ attribute
linkedinHref_ :: T.Text -> Attribute
linkedinHref_ = makeAttribute "linkedin-href"

-- | @linkedin-icon-color@ attribute
linkedinIconColor_ :: T.Text -> Attribute
linkedinIconColor_ = makeAttribute "linkedin-icon-color"

-- | @mode@ attribute
mode_ :: T.Text -> Attribute
mode_ = makeAttribute "mode"

-- | @padding@ attribute
padding_ :: T.Text -> Attribute
padding_ = makeAttribute "padding"

-- | @padding-bottom@ attribute
paddingBottom_ :: T.Text -> Attribute
paddingBottom_ = makeAttribute "padding-bottom"

-- | @padding-left@ attribute
paddingLeft_ :: T.Text -> Attribute
paddingLeft_ = makeAttribute "padding-left"

-- | @padding-right@ attribute
paddingRight_ :: T.Text -> Attribute
paddingRight_ = makeAttribute "padding-right"

-- | @padding-top@ attribute
paddingTop_ :: T.Text -> Attribute
paddingTop_ = makeAttribute "padding-top"

-- | @pinterest-content@ attribute
pinterestContent_ :: T.Text -> Attribute
pinterestContent_ = makeAttribute "pinterest-content"

-- | @pinterest-href@ attribute
pinterestHref_ :: T.Text -> Attribute
pinterestHref_ = makeAttribute "pinterest-href"

-- | @pinterest-icon-color@ attribute
pinterestIconColor_ :: T.Text -> Attribute
pinterestIconColor_ = makeAttribute "pinterest-icon-color"

-- | @price@ attribute
price_ :: T.Text -> Attribute
price_ = makeAttribute "price"

-- | @quantity@ attribute
quantity_ :: T.Text -> Attribute
quantity_ = makeAttribute "quantity"

-- | @right-icon@ attribute
rightIcon_ :: T.Text -> Attribute
rightIcon_ = makeAttribute "right-icon"

-- | @table-layout@ attribute
tableLayout_ :: T.Text -> Attribute
tableLayout_ = makeAttribute "table-layout"

-- | @tb-border@ attribute
tbBorder_ :: T.Text -> Attribute
tbBorder_ = makeAttribute "tb-border"

-- | @tb-border-radius@ attribute
tbBorderRadius_ :: T.Text -> Attribute
tbBorderRadius_ = makeAttribute "tb-border-radius"

-- | @tb-hover-border-color@ attribute
tbHoverBorderColor_ :: T.Text -> Attribute
tbHoverBorderColor_ = makeAttribute "tb-hover-border-color"

-- | @tb-selected-border-color@ attribute
tbSelectedBorderColor_ :: T.Text -> Attribute
tbSelectedBorderColor_ = makeAttribute "tb-selected-border-color"

-- | @tb-width@ attribute
tbWidth_ :: T.Text -> Attribute
tbWidth_ = makeAttribute "tb-width"

-- | @text-align@ attribute
textAlign_ :: T.Text -> Attribute
textAlign_ = makeAttribute "text-align"

-- | @text-decoration@ attribute
textDecoration_ :: T.Text -> Attribute
textDecoration_ = makeAttribute "text-decoration"

-- | @text-mode@ attribute
textMode_ :: T.Text -> Attribute
textMode_ = makeAttribute "text-mode"

-- | @text-transform@ attribute
textTransform_ :: T.Text -> Attribute
textTransform_ = makeAttribute "text-transform"

-- | @thumbnails@ attribute
thumbnails_ :: T.Text -> Attribute
thumbnails_ = makeAttribute "thumbnails"

-- | @thumbnails-src@ attribute
thumbnailsSrc_ :: T.Text -> Attribute
thumbnailsSrc_ = makeAttribute "thumbnails-src"

-- | @twitter-content@ attribute
twitterContent_ :: T.Text -> Attribute
twitterContent_ = makeAttribute "twitter-content"

-- | @twitter-href@ attribute
twitterHref_ :: T.Text -> Attribute
twitterHref_ = makeAttribute "twitter-href"

-- | @twitter-icon-color@ attribute
twitterIconColor_ :: T.Text -> Attribute
twitterIconColor_ = makeAttribute "twitter-icon-color"

-- | @vertical-align@ attribute
verticalAlign_ :: T.Text -> Attribute
verticalAlign_ = makeAttribute "vertical-align"


 -- Legacy attributes
-- | @bgcolor@ HTML attribute
bgcolor_ :: T.Text -> Attribute
bgcolor_ = makeAttribute "bgcolor"

-- | @valign@ HTML attribute
valign_ :: T.Text -> Attribute
valign_ = makeAttribute "valign"
