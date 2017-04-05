{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}

module Lucid.PreEscaped where

import Lucid
import Lucid.Base
import qualified Data.Text as T
import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import Data.Monoid ((<>))


preEscaped :: Monad m => T.Text -> HtmlT m ()
preEscaped name =
  HtmlT (return (\attr -> Blaze.fromText name, ()))


scriptSrc :: Monad m => T.Text -> HtmlT m ()
scriptSrc url =
  HtmlT (return (\attr -> "<script src=\"" <> Blaze.fromHtmlEscapedText url<>"\"></script>", ()))
