{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables#-}

module Lucid.Rdash (indexPage) where

import Control.Monad

import Lucid

rdashCSS, bsCSS, cdnFontAwesome, sidebarMain, sidebarTitle :: Monad m => HtmlT m ()

rdashCSS = link_ [rel_ "stylesheet",
                  href_  "css/rdash.min.css"]

bsCSS = link_ [rel_ "stylesheet",
               href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"]

cdnFontAwesome
  =  link_ [href_ "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css",
            rel_ "stylesheet",
            type_ "text/css"]

sidebarTitle = span_ "NAVIGATION"
sidebarMain  = a_ [href_ "#"] $ do
  "Dashboard"
  span_ [class_ "menu-icon glyphicon glyphicon-transfer"] (return ())

mkPageWrapperOpen :: (Monad m) => HtmlT m () -> HtmlT m ()
mkPageWrapperOpen sbw = div_ [id_ "page-wrapper", class_ "open"] sbw

mkSidebarWrapper :: (Monad m) => HtmlT m () -> HtmlT m () -> HtmlT m ()
mkSidebarWrapper sb sbf = div_ [id_ "sidebar-wrapper"] $ sb >> sbf

mkSidebar :: (Monad m) => HtmlT m () -> HtmlT m () -> [HtmlT m ()] -> HtmlT m ()
mkSidebar sbm sbt sbl = ul_ [class_ "sidebar"] $ do
  li_ [class_ "sidebar-main"] sbm
  li_ [class_ "sidebar-title"] sbt
  forM_ sbl $ \l -> do
    li_ [class_ "sidebar-list"] l

mkSidebarLiContent :: (Monad m) => String -> HtmlT m ()
mkSidebarLiContent s = a_ [href_ "#"] $ (toHtml s) >> span_ [class_ "menu-icon fa fa-tachometer"] (return ())

mkSidebarFooter :: (Monad m) => [HtmlT m ()] -> HtmlT m ()
mkSidebarFooter footerItems = div_ [class_ "sidebar-footer"] $ mapM_ id footerItems

mkColXS4 :: Monad m => HtmlT m () -> HtmlT m ()
mkColXS4 c = div_ [class_ "col-xs-4"] c

mkHead :: (Monad m) => String -> HtmlT m ()
mkHead title = head_ $ do
  meta_ [charset_ "UTF-8"]
  meta_ [name_ "viewport", content_ "width=device-width"] -- TODO: add attribute initial-scale=1
  title_ (toHtml title)
  rdashCSS
  cdnFontAwesome
  bsCSS

mkBody :: (Monad m) => HtmlT m () -> HtmlT m ()
mkBody pgw = body_ [class_ "hamburg"] pgw

mkIndexPage :: (Monad m) => HtmlT m () -> HtmlT m () -> HtmlT m ()
mkIndexPage hd body = html_ [lang_ "en"] $ hd >> body

indexPage :: (Monad m) => [String] -> HtmlT m ()
indexPage navItems = do
  mkIndexPage hd body
  where
    sb = mkSidebar sidebarMain sidebarTitle (mkSidebarLiContent <$> navItems)

    githubFooter = mkColXS4 $ a_ [href_ "https://github.com/rdash/rdash-barebones", target_ "blank_"] "Github"
    aboutFooter = mkColXS4 $ a_ [href_ "#", target_ "blank_"] "About"
    supportFooter = mkColXS4 $ a_ [href_ "#"] "Support"

    sbf = mkSidebarFooter $ [githubFooter, aboutFooter, supportFooter]
    sbw = mkSidebarWrapper sb sbf
    pgw = mkPageWrapperOpen sbw
    body = mkBody pgw

    hd = mkHead ("Dashboard" :: String)
