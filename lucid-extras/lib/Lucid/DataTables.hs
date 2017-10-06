{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}

module Lucid.DataTables where

import Lucid
import qualified Data.Text as T
import Lucid.PreEscaped
import Data.Monoid

dataTablesCDN :: Monad m => HtmlT m ()
dataTablesCDN
  =  scriptSrc "https://cdn.datatables.net/1.10.16/js/jquery.dataTables.min.js"

activateDataTable :: Monad m => T.Text -> HtmlT m ()
activateDataTable elm = script_ $
  "$(document).ready(function(){ $('"<> elm<>"').DataTable(); })"

dataTablesCssCDN :: Monad m => HtmlT m ()
dataTablesCssCDN =
  link_ [rel_ "stylesheet",
         href_ "https://cdn.datatables.net/1.10.16/css/jquery.dataTables.min.css"]