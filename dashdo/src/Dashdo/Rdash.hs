{-# LANGUAGE OverloadedStrings #-}

module Dashdo.Rdash (rdash, charts, controls) where

import Dashdo
import Dashdo.Types
import Lucid
import Lucid.Bootstrap3
import qualified Lucid.Rdash as RD
import qualified Data.Text.Lazy as TL
import Data.Text hiding (map, intersperse, length)
import Data.Monoid
import Data.List (intersperse)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (forM_)

sidebarMain, sidebarTitle :: (Monad m) => HtmlT m ()
sidebarTitle = span_ "Dashboards"
sidebarMain  = a_ [href_ "#"] $ do
  "Dashdo"
  span_ [class_ "menu-icon glyphicon glyphicon-transfer"] (return ())

sidebarItem :: Monad m => (String, Text) -> HtmlT m ()
sidebarItem (fid, title) = do
  a_ [class_ "dashdo-link", href_ (pack fid)] $ do
    toHtml title
    span_ [class_ "fa fa-tachometer menu-icon"] (return ())

dashdo :: RDashdo -> IO (String, TL.Text)
dashdo (RDashdo fid d) = do
  t <- fst <$> dashdoGenOut d (initial d)
  return (fid, t)

rdash :: Html () -> [(Text, RDashdo)] -> IO TL.Text
rdash headExtra ds = do
  dashdos <- mapM (dashdo . snd) ds
  return $ renderText $ doctypehtml_ $ do
      head_ $ do
        meta_ [charset_ "utf-8"]
        cdnCSS
        cdnThemeCSS
        headExtra
      body_ $ do
        let form'_ = \(fid, txt) -> form_ [class_ "dashdo", id_ (pack fid)] $ (toHtmlRaw txt)
            titles = (map (rdFid . snd &&& fst) ds)
            sbl = map sidebarItem titles
            sb  = RD.mkSidebar sidebarMain sidebarTitle sbl
            sbf = RD.mkSidebarFooter (rowEven XS
              [ a_ [href_ "https://github.com/filopodia/open"] (i_ [class_ "fa fa-lg fa-github"] mempty <> "Github")
              , a_ [href_ "#"] $ i_ [id_ "spinner", class_ "fa fa-cog fa-2x"] mempty
              ])
            sbw = RD.mkSidebarWrapper sb sbf
            hdr ts = RD.mkHeaderBar [RD.mkMetaBox [RD.mkMetaTitle
              (mapM_ (\(f, t) -> span_ [class_ "dashdo-title", id_ (pack f <> "-title")] (toHtml t)) ts)]]
            cw  = RD.mkPageContent $ do
              hdr titles
              (mapM_ form'_ dashdos)
            pgw = RD.mkPageWrapperOpen sbw cw
        RD.mkIndexPage (RD.mkHead "Dashdo") (RD.mkBody pgw)
        cdnJqueryJS
        cdnBootstrapJS
        script_ [src_ "/js/dashdo.js"] ("" :: Text)

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
    widget = \(title, content) -> do
      mkCol [(XS, 12), (MD, 12 `div` length cs)] $
        div_ [class_ "widget"] $ do
          div_ [class_ "widget-header"] (toHtml title)
          div_ [class_ "widget-body no-padding"] $
            div_ [class_ "widget-content"] $
              content
