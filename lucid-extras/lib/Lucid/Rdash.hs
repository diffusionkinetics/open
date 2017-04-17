{-# LANGUAGE OverloadedStrings #-}

module Lucid.Rdash (mkIndexPage) where

import Control.Monad

import Lucid

rdashCSS, sidebarMain, sidebarTitle :: Monad m => HtmlT m ()

rdashCSS = link_ [rel_ "stylesheet",
                  type_ "text/css",
                  href_  "css/rdash.min.css"]

sidebarTitle = span_ "NAVIGATION"
sidebarMain  = a_ $ do
  "Dashboard"
  span_ [class_ "menu-icon"] (return ())

mkIndexPage :: (Monad m) => [HtmlT m ()] -> [HtmlT m ()] -> HtmlT m ()
mkIndexPage navItems footerItems = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    rdashCSS
  body_ $ do
    div_ [id_ "page-wrapper"] $ do
      mkSidebarWrapper navItems footerItems

mkSidebarWrapper :: (Monad m) => [HtmlT m ()] -> [HtmlT m ()] -> HtmlT m ()
mkSidebarWrapper navItems footerItems = div_ [id_ "sidebar-wrapper"] $ do
  mkSidebar sidebarMain sidebarTitle navItems
  mkSidebarFooter footerItems

mkSidebar :: (Monad m) => HtmlT m () -> HtmlT m () -> [HtmlT m ()] -> HtmlT m ()
mkSidebar m t ls = ul_ [class_ "sidebar"] $ do
  li_ [class_ "sidebar-main"] m
  li_ [class_ "sidebar-title"] t
  forM_ ls $ \l -> do
    li_ [class_ "sidebar-list"] l

mkSidebarFooter :: (Monad m) => [HtmlT m ()] -> HtmlT m ()
mkSidebarFooter footerItems = div_ [class_ "sidebar-footer"] $ mapM_ id footerItems
