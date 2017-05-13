{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Mjml.Skeleton where

import Lucid.Mjml.Component

import Lucid.Base
import Lucid.Html5

import qualified Data.Text as T

import Data.Monoid

import Control.Monad.Morph
import Control.Monad.State

import qualified Data.HashMap.Strict as HM

import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Blaze.ByteString.Builder (Builder)

style :: T.Text
style = T.concat [
  "#outlook a { padding:0; }"
  , ".ReadMsgBody { width:100%; }"
  , ".ExternalClass { width:100%; }"
  , ".ExternalClass * { line-height:100%; }"
  , "body { margin:0;padding:0;-webkit-text-size-adjust:100%;-ms-text-size-adjust:100%; }"
  , "table, td { border-collapse:collapse;mso-table-lspace:0pt;mso-table-rspace:0pt; }"
  , "img { border:0;height:auto;line-height:100%; outline:none;text-decoration:none;-ms-interpolation-mode:bicubic; }"
  , "p { display:block;margin:13px 0; }"
  ]

styleConditional :: T.Text
styleConditional = T.concat [
  "@media only screen and (max-width:480px) {"
  , "@-ms-viewport { width:320px; }"
  , "@viewport { width:320px; }}"
  ]

msoXml :: Monad m => MjmlT m ()
msoXml = term "xml" $ do
  term "o:OfficeDocumentSettings" $ do
    makeXmlElementNoEnd ("o:AllowPNG" :: T.Text)
    term "o:PixelsPerInch" "96"

buildFontTags :: Monad m => MjmlT m ()
buildFontTags = do
  fs <- fonts <$> get
  build "<!--[if !mso]><!-->"
  forM_ (HM.elems fs) $ \url -> do
    link_ [href_ url, rel_ "stylesheet", type_ "text/css"]
    style_ [type_ "text/css"] $ T.concat ["@url(", url , ");"]

buildMediaQueriesTags :: Monad m => MjmlT m ()
buildMediaQueriesTags = do
  mqs <- mediaQueries <$> get
  let mqContent = T.concat $ HM.foldlWithKey' (\acc cn mq -> mediaQueryTag cn mq : acc) [] mqs
  style_ [type_ "text/css"] $ T.concat ["@media only screen and (min-width:480px) {", mqContent, "}"]
  where
    mediaQueryTag className mediaQuery = T.concat [".", className, " ", mediaQuery]

skeleton :: Monad m => MjmlT m () -> MjmlT m ()
skeleton content = do
  -- Hack to build fonts and media queries without running the content action.
  (contentBuilder, st) <- lift . lift $ runStateT (execHtmlT content) emptyState

  doctype_
  html_ [ xmlns_ "http://www.w3.org/1999/xhtml"
        , (term "xmlns:v") "urn:schemas-microsoft-com:vml"
        , (term "xmlns:o") "urn:schemas-microsoft-com:office:office"] $ do
    head_ $ do
      title_ "Title"
      build "<!--[if !mso]><!-- -->"
      meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge"]
      build "<!--<![endif]-->"
      meta_ [httpEquiv_ "ContentType", content_"text/html; charset=UTF-8"]
      style_ [type_ "text/css"] style

      build "<!--[if !mso]><!-- -->"
      style_ [type_ "text/css"] styleConditional
      build "<!--<![endif]-->"

      build "<!--[if mso]>"
      msoXml
      build "<![endif]-->"

      build "<!--[if lte mso 11]>"
      style_ [type_ "text/css"] (".outlook-group-fix { width:100% !important; }" :: T.Text)
      build "<![endif]-->"

      -- Hack to build fonts and media queries without running the content action.
      put st
      buildFontTags
      buildMediaQueriesTags

    body_ $ build contentBuilder
