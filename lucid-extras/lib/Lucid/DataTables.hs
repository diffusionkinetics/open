{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}

module Lucid.DataTables where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Lucid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Lucid.PreEscaped
import Data.Monoid

dataTablesCDN :: Monad m => HtmlT m ()
dataTablesCDN
  =  scriptSrc "https://cdn.datatables.net/1.10.16/js/jquery.dataTables.min.js"

activateDataTable :: Monad m => T.Text
                     -- | Additional properties to pass to the table.
                     --   See https://datatables.net/reference/option/ for values
                     --   we might wish to use here.
                  -> Maybe (Aeson.Value)
                  -> HtmlT m ()
activateDataTable elm props = script_ $
    "$(document).ready(function(){ $('"<> elm<>"').DataTable("<> propStr <> "); })"
  where
    propStr = case props of
      Just val -> T.decodeUtf8 . BSL.toStrict . Aeson.encode $ val
      Nothing -> ""

dataTablesCssCDN :: Monad m => HtmlT m ()
dataTablesCssCDN =
  link_ [rel_ "stylesheet",
         href_ "https://cdn.datatables.net/1.10.16/css/jquery.dataTables.min.css"]
