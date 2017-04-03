{-# LANGUAGE OverloadedStrings #-}
module Lucid.Bootstrap3 where

import Lucid
import Lucid.Base (makeElementNoEnd)
import Data.Char (toLower)
import Data.Text (pack, Text)
import Data.Monoid (mempty)
infixr 0 $:

($:) :: (Monad m, ToHtml a) => (HtmlT m () -> HtmlT m ()) -> a -> HtmlT m ()
f $: x = f (toHtml x)


data Breakpoint = XS | SM | MD | LG deriving Show

rowEven :: Monad m => Breakpoint -> [HtmlT m ()] -> HtmlT m ()
rowEven bp cols = div_ [class_ "row"] $ do
  let ncols = length cols
      spans = 12 `div` ncols
      cls = pack $ concat ["col-", map toLower (show bp),"-",show spans]
  mapM_ (div_ [class_ cls]) cols

cdnCSS, cdnThemeCSS, cdnJqueryJS, cdnBootstrapJS ::  Monad m => HtmlT m ()
cdnCSS
  = link_ [rel_ "stylesheet",
           href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"]

cdnThemeCSS
  = link_ [rel_ "stylesheet",
           href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"]

cdnJqueryJS
  =  scriptSrc "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"

cdnBootstrapJS
  =  scriptSrc "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"


scriptSrc :: Monad m => Text -> HtmlT m ()
scriptSrc url = with (makeElementNoEnd "script") [src_ url]
