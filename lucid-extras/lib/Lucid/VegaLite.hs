{-# language OverloadedStrings, ExtendedDefaultRules #-}
{-|
Module      : Lucid.VegaLite
Description : Vega-lite bindings
Copyright   : (c) Tom Nielsen, Marco Zocca, 2019
License     : GPL-3
Maintainer  : ocramz fripost org
Stability   : experimental
Portability : POSIX

Bindings to the vega-lite visualization and infographics API.

See https://vega.github.io/ for usage details
-}
module Lucid.VegaLite (
  -- * Standalone
  mkVegaHtml
  -- * Utilities
  , vegaEmbedHead, vegaEmbedBodyScript) where

import Lucid
import Lucid.PreEscaped (scriptSrc)

import qualified Data.Aeson as A

-- import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8)

import qualified Data.ByteString.Lazy as LBS

import Data.Monoid

vegaCDN, vegaLiteCDN, vegaEmbedCDN :: Monad m => HtmlT m ()
vegaCDN = scriptSrc "https://cdn.jsdelivr.net/npm/vega@3"
vegaLiteCDN = scriptSrc "https://cdn.jsdelivr.net/npm/vega-lite@2.5.0"
vegaEmbedCDN = scriptSrc "https://cdn.jsdelivr.net/npm/vega-embed@3"

-- | Construct a standalone HTML page that can render a vega-lite plot. The plot will be rendered by the vega-embed library.
--
-- NB: the 'A.Value' parameter must contain a vega-lite JSON payload
mkVegaHtml :: A.Value -> Html ()
mkVegaHtml vl = doctypehtml_ $ html_ $ do
  meta_ [charset_ "UTF-8"]
  head_ vegaEmbedHead
  with div_ [id_ "vis"] ""
  body_ $ vegaEmbedBodyScript vl 

-- | The statements for downloading the vega javascript blobs from the CDN. Must be in the document \<HEAD\>
vegaEmbedHead :: Html ()
vegaEmbedHead = do
    vegaCDN
    vegaLiteCDN
    vegaEmbedCDN

-- | The statement for embedding the vega JSON payload and initializing vega-embed. Must be in the \<BODY\> block and referenced by a \<div id="vis"\>\</div\>
vegaEmbedBodyScript :: A.Value -> Html ()
vegaEmbedBodyScript vl = 
    script_ $ T.decodeUtf8 $ LBS.toStrict ("const spec =" <> A.encode vl <> "; vegaEmbed('#vis', spec).then(result => console.log(result)).catch(console.warn);" :: LBS.ByteString)     
