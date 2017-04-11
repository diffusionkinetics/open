{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Lucid.Bootstrap3 where

import Lucid
import Lucid.PreEscaped (scriptSrc)
import Data.Char (toLower)
import Data.Text (pack, Text)
import qualified Data.Text as T
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

cdnCSS, cdnThemeCSS, cdnJqueryJS, cdnBootstrapJS, cdnFontAwesome ::  Monad m => HtmlT m ()
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

cdnFontAwesome
  =  link_ [href_ "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css",
            rel_ "stylesheet",
            type_ "text/css"]

data NavAttribute = Inverse | Transparent | FixedTop | NavBarClass Text deriving Eq

navAttributeToClass Inverse = "navbar-inverse"
navAttributeToClass Transparent = "navbar-transparent"
navAttributeToClass FixedTop = "navbar-fixed-top"
navAttributeToClass (NavBarClass c)= c

navBar :: [NavAttribute] -> Html () -> [Html ()] -> Html ()
navBar attrs brand items = do
  let cls = T.unwords $ "navbar" : map navAttributeToClass attrs
  nav_ [class_ cls, role_ "navigation"] $ div_ [class_ "container"] $ do
    div_ [class_ "navbar-header"] $ do
      button_ [id_ "menu-toggle",
               type_ "button",
               class_ "navbar-toggle"] $ do
        span_ [class_ "sr-only"] "Toggle navigation"
        span_ [class_ "icon-bar bar1"] ""
        span_ [class_ "icon-bar bar2"] ""
        span_ [class_ "icon-bar bar3"] ""
      with brand [class_ "navbar-brand"]
    div_ [class_ "collapse navbar-collapse"] $ do
      ul_ [class_ "nav navbar-nav navbar-right"] $ do
        mapM_ li_ items
