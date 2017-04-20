{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables#-}

module Lucid.Rdash (indexPage) where

import Data.List

import Control.Monad

import Lucid.Bootstrap3

import Lucid

rdashCSS, sidebarMain, sidebarTitle :: Monad m => HtmlT m ()

rdashCSS = link_ [rel_ "stylesheet",
                  href_  "http://cdn.filopodia.com/rdash-ui/1.0.1/css/rdash.css"]

sidebarTitle = span_ "NAVIGATION"
sidebarMain  = a_ [href_ "#"] $ do
  "Dashboard"
  span_ [class_ "menu-icon glyphicon glyphicon-transfer"] (return ())

mkPageWrapperOpen :: (Monad m) => HtmlT m () -> HtmlT m () -> HtmlT m ()
mkPageWrapperOpen sbw cw = div_ [id_ "page-wrapper", class_ "open"] $ sbw >> cw

mkSidebarWrapper :: (Monad m) => HtmlT m () -> HtmlT m () -> HtmlT m ()
mkSidebarWrapper sb sbf = div_ [id_ "sidebar-wrapper"] $ sb >> sbf

mkSidebar :: (Monad m) => HtmlT m () -> HtmlT m () -> [HtmlT m ()] -> HtmlT m ()
mkSidebar sbm sbt sbl = ul_ [class_ "sidebar"] $ do
  li_ [class_ "sidebar-main", id_ "toggle-sidebar"] sbm
  li_ [class_ "sidebar-title"] sbt
  forM_ sbl $ \l -> do
    li_ [class_ "sidebar-list"] l

mkSidebarLiContent :: (Monad m) => String -> HtmlT m ()
mkSidebarLiContent s = a_ [href_ "#"] $ (toHtml s) >> span_ [class_ "menu-icon fa fa-tachometer"] (return ())

mkSidebarFooter :: (Monad m) => HtmlT m () -> HtmlT m ()
mkSidebarFooter footerItems = div_ [class_ "sidebar-footer"] footerItems

mkHead :: (Monad m) => String -> HtmlT m ()
mkHead title = head_ $ do
  meta_ [charset_ "UTF-8"]
  meta_ [name_ "viewport", content_ "width=device-width"] -- TODO: add attribute initial-scale=1
  title_ (toHtml title)
  rdashCSS
  cdnFontAwesome
  cdnCSS
  cdnJqueryJS
  cdnBootstrapJS

mkBody :: (Monad m) => HtmlT m () -> HtmlT m ()
mkBody pgw = body_ [class_ "hamburg"] pgw

mkPageContent :: Monad m => HtmlT m () -> HtmlT m ()
mkPageContent = div_ [id_ "content-wrapper"] . div_ [class_ "page-content"]

mkHeaderBar :: Monad m => [HtmlT m ()] -> HtmlT m ()
mkHeaderBar cols = div_ [class_ "row header"] (rowEven XS [mkUserBox cols])

mkUserBox :: Monad m => [HtmlT m ()] -> HtmlT m ()
mkUserBox xs = div_ [class_ "user pull-right"] $ sequence_ xs

mkItemDropdown :: Monad m => HtmlT m () -> HtmlT m ()
mkItemDropdown x = div_ [class_ "item dropdown"] $ do
  a_ [ class_ "dropdown-toggle"
     , term "data-toggle" $ "dropdown"
     , href_ "#"] (p_ "Click Here!")
  x

mkDropdownMenu :: Monad m => HtmlT m () -> [[HtmlT m ()]] -> HtmlT m ()
mkDropdownMenu hdr xs = ul_ [class_ "dropdown-menu dropdown-menu-right"] $ sequence_ dividedItems
  where
    mkLi = li_ . (a_ [href_ "#"])
    items = map (map mkLi) xs
    dividedItems = (li_ [class_ "dropdown-header"] hdr) : li_ [class_ "divider"] (return ()) :
      (concat $ intersperse [li_ [class_ "divider"] (return ())] items)

mkIndexPage :: (Monad m) => HtmlT m () -> HtmlT m () -> HtmlT m ()
mkIndexPage hd body = html_ [lang_ "en"] $ hd >> body

indexPage :: (Monad m) => [String] -> HtmlT m ()
indexPage navItems = do
  mkIndexPage hd body
  where
    sb = mkSidebar sidebarMain sidebarTitle (mkSidebarLiContent <$> navItems)

    footerContent =
      rowEven XS
      [ a_ [href_ "https://github.com/rdash/rdash-barebones", target_ "blank_"] "Github"
      , a_ [href_ "#", target_ "blank_"] "About"
      , a_ [href_ "#"] "Support"]

    sbf = mkSidebarFooter footerContent
    sbw = mkSidebarWrapper sb sbf

    notifMenu = mkItemDropdown $ mkDropdownMenu "Joe Bloggs" [["Profile", "Menu Item"], ["Logout"]]

    hb = mkHeaderBar [notifMenu]
    pcw = mkPageContent hb

    pgw = mkPageWrapperOpen sbw pcw

    body = mkBody pgw

    hd = mkHead ("Dashboard" :: String)
