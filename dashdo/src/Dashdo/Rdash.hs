{-# LANGUAGE OverloadedStrings #-}

module Dashdo.Rdash (rdash, charts, controls, inSidebar) where

import Dashdo
import Dashdo.Types
import Lucid
import Lucid.Bootstrap3
import qualified Lucid.Rdash as RD
import qualified Data.Text.Lazy as TL
import Data.Text hiding (map, intersperse, length)
import Data.Monoid
import Control.Applicative ((<$>))

sidebarMain, sidebarTitle :: (Monad m) => HtmlT m ()
sidebarTitle = span_ "Dashboards"
sidebarMain  = a_ [href_ "#"] $ do
  "Dashdo"
  span_ [class_ "menu-icon glyphicon glyphicon-transfer"] (return ())

sidebarList :: (Monad m) => [RDashdo] -> [HtmlT m ()]
sidebarList = map (\d->a_ [href_ (pack $ rdFid d), class_ "dashdo-link"] $ (toHtml $ rdTitle d) <> (i_ [class_ "fa fa-tachometer menu-icon"] mempty))

dashdo :: RDashdo -> IO (String, TL.Text)
dashdo (RDashdo fid _ d) = do
  t <- fst <$> dashdoGenOut d (initial d)
  return (fid, t)

rdash :: [RDashdo] -> Html () -> IO TL.Text
rdash rdashdos headExtra = do
  return $ renderText $ doctypehtml_ $ do
      head_ $ do
        meta_ [charset_ "utf-8"]
        cdnCSS
        cdnThemeCSS
        headExtra
      body_ $ do
        let sb  = do RD.mkSidebar sidebarMain sidebarTitle $ sidebarList rdashdos
            sbf = RD.mkSidebarFooter (rowEven XS
              [ a_ [href_ "https://github.com/filopodia/open"] (i_ [class_ "fa fa-lg fa-github"] mempty <> "Github")
              , a_ [href_ "#"] $ i_ [id_ "spinner", class_ "fa fa-cog fa-2x"] mempty
              ])
            sbw = RD.mkSidebarWrapper sb sbf
            cw  = RD.mkPageContent $ do
              RD.mkHeaderBar [RD.mkMetaBox [RD.mkMetaTitle (span_ [id_ "dashdo-title"] mempty)]]
              div_ [id_ "dashdo-main"] mempty
            pgw = form_ [id_ "dashdo-form", method_ "post"] $ RD.mkPageWrapperOpen sbw cw
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

inSidebar :: SHtml a () -> SHtml a ()
inSidebar = div_ [class_ "dashdo-sidebar"]
