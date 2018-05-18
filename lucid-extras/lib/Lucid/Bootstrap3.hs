{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Lucid.Bootstrap3 where

import Lucid
import Lucid.PreEscaped (scriptSrc)
import Data.Char (toLower)
import qualified Data.Text as T
infixr 0 $:

($:) :: (Monad m, ToHtml a) => (HtmlT m () -> HtmlT m ()) -> a -> HtmlT m ()
f $: x = f (toHtml x)

data Breakpoint = XS | SM | MD | LG deriving Show

mkColClass :: [(Breakpoint, Int)] -> T.Text
mkColClass = T.unwords . map go
  where
    go (bp, spans) = T.concat [ "col-", T.pack $ map toLower (show bp)
                              , "-", T.pack $ show spans]

mkCol :: Monad m => [(Breakpoint, Int)] -> HtmlT m () -> HtmlT m ()
mkCol bps = div_ [class_ (mkColClass bps)]

rowEven :: Monad m => Breakpoint -> [HtmlT m ()] -> HtmlT m ()
rowEven bp cols = mapM_ (div_ [class_ (mkColClass [(bp, spans)])]) cols
  where ncols = length cols
        spans = 12 `div` ncols

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
  =  link_ [href_ "https://maxcdn.bootstrapcdn.com/font-awesome/5.0.13/css/font-awesome.min.css",
            rel_ "stylesheet",
            type_ "text/css"]

data NavAttribute = Inverse | Transparent | FixedTop | NavBarClass T.Text deriving Eq

navAttributeToClass :: NavAttribute -> T.Text
navAttributeToClass Inverse = "navbar-inverse"
navAttributeToClass Transparent = "navbar-transparent"
navAttributeToClass FixedTop = "navbar-fixed-top"
navAttributeToClass (NavBarClass c)= c

navBar :: Monad m => [NavAttribute] -> HtmlT m () -> [HtmlT m ()] -> HtmlT m ()
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

loginForm :: Monad m => T.Text -> Maybe (HtmlT m ()) -> HtmlT m ()
loginForm url mwarn = form_ [class_ "form-signin", method_ "post", action_ url] $ do
  h2_ [class_ "form-signin-heading"] $ "Please sign in"
  case mwarn of
    Nothing -> return ()
    Just warn -> warn
  label_ [for_ "inputEmail", class_ "sr-only"] "Email address"
  input_ [type_ "email", id_ "inputEmail", name_ "inputEmail", class_ "form-control", placeholder_ "Email address", required_ "", autofocus_]
  label_ [for_ "inputPassword", class_ "sr-only"] "Password"
  input_ [type_ "password", id_ "inputPassword", name_ "inputPassword", class_ "form-control", placeholder_ "Password", required_ ""]
  div_ [class_ "checkbox"] $
    label_ $ do
      input_ [type_ "checkbox", value_ "remember-me"]
      "Remember me"
  button_ [class_ "btn btn-lg btn-primary btn-block", type_ "submit"] "Sign in"

