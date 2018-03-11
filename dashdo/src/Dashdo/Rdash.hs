{-# LANGUAGE OverloadedStrings #-}

module Dashdo.Rdash (rdash, charts, controls, defaultSidebar, Sidebar(..)) where

import Dashdo.Types
import Lucid
import Lucid.Bootstrap3
import qualified Lucid.Rdash as RD
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text hiding (map, intersperse, length)
import Data.Monoid
import Control.Applicative ((<$>))

sidebarMain :: Sidebar -> Html ()
sidebarMain sb = a_ [href_ "#"] $ do
  toHtml $ title sb
  span_ [class_ "menu-icon glyphicon glyphicon-transfer"] (return ())

sidebarList :: [RDashdo m] -> [RD.SidebarItem]
sidebarList rdashdos = (sidebarListItem <$> rdashdos) -- <> [div_ [id_ "dashdo-sidebar"] mempty ]
  where
    sidebarListItem = \rd ->
      RD.SidebarLink (rdTitle rd) (pack $ rdFid rd) "tachometer"


data Sidebar = Sidebar
  {  title:: T.Text
  ,  subTitle:: T.Text
  ,  footer:: Html ()
  }

defaultSidebar :: Sidebar
defaultSidebar = Sidebar "Dashdo" "Dashboards" $ rowEven XS
              [ a_ [href_ "https://github.com/diffusionkinetics/open/dashdo"] (i_ [class_ "fa fa-lg fa-github"] mempty <> "Github")
              , a_ [href_ "#"] $ i_ [id_ "spinner", class_ "fa fa-cog fa-2x"] mempty
              ]


rdash :: [RDashdo m] -> Html () -> Sidebar -> TL.Text
rdash rdashdos headExtra sb = do
  renderText $ doctypehtml_ $ do
      head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width"] -- TODO: add attribute initial-scale=1
        -- If href doesn't end with a slash, redirect to the one with slashes
        -- this is needed to make relative urls work
        script_ $ T.unlines
            [ "if (!window.location.href.match(/\\/$/)) {"
            , "  window.location.href = window.location.href + '/';"
            , "}"
            ]
        cdnFontAwesome
        cdnCSS
        RD.rdashCSS
        cdnJqueryJS
        headExtra
      body_ $ do
        let sb'  = (RD.mkSidebar (sidebarMain sb) $ sidebarList rdashdos) <> div_ [id_ "dashdo-sidebar"] mempty
            sbf = RD.mkSidebarFooter $ footer sb
            sbw = RD.mkSidebarWrapper sb' sbf
            cw  = RD.mkPageContent $ do
              RD.mkHeaderBar [RD.mkMetaBox [RD.mkMetaTitle (span_ [id_ "dashdo-title"] mempty)]]
              div_ [id_ "dashdo-main"] mempty
            pgw = form_ [id_ "dashdo-form", method_ "post"] $ RD.mkPageWrapperOpen sbw cw
        --RD.mkIndexPage (RD.mkHead "Dashdo") (
        pgw
        cdnBootstrapJS
        script_ [src_ "js/dashdo.js"] ("" :: Text)
        script_ [src_ "js/runners/rdashdo.js"] ("" :: Text)

controls :: Monad m => HtmlT m () -> HtmlT m ()
controls content = do
  div_ [class_ "row"] $
    mkCol [(XS, 12), (MD, 12)] $
      div_ [class_ "widget"] $ do
        div_ [class_ "widget-header"] "Settings"
        div_ [class_ "widget-body"] $
          div_ [class_ "widget-content"] $
            content

charts :: Monad m =>[(Text, HtmlT m ())] -> HtmlT m ()
charts cs = do
  div_ [class_ "row"] $ sequence_ widgets
  where
    widgets = map widget cs
    widget = \(titl, content) -> do
      mkCol [(XS, 12), (MD, 12 `div` length cs)] $
        div_ [class_ "widget"] $ do
          div_ [class_ "widget-header"] (toHtml titl)
          div_ [class_ "widget-body no-padding"] $
            div_ [class_ "widget-content"] $
              content
