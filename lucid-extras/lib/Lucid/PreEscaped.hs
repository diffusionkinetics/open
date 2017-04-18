{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}

module Lucid.PreEscaped where

import Lucid
import Lucid.Base
import qualified Data.Text as T
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as LBS


preEscaped :: Monad m => T.Text -> HtmlT m ()
preEscaped name =
  HtmlT (return (\_ -> Blaze.fromText name, ()))

preEscapedByteString :: Monad m => LBS.ByteString -> HtmlT m ()
preEscapedByteString name =
  HtmlT (return (\_ -> Blaze.fromLazyByteString name, ()))



scriptSrc :: Monad m => T.Text -> HtmlT m ()
scriptSrc url =
  HtmlT (return (\_ -> "<script src=\"" <> Blaze.fromHtmlEscapedText url<>"\"></script>", ()))
