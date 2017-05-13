{-# LANGUAGE OverloadedStrings #-}

module Dashdo.Rdash (rdash) where

import Dashdo
import Dashdo.Types
import Lucid
import Lucid.Bootstrap3
import qualified Lucid.Rdash as RD
import qualified Data.Text.Lazy as TL
import Data.Text hiding (map)
import Data.Monoid
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

sidebarMain, sidebarTitle :: (Monad m) => HtmlT m ()
sidebarTitle = span_ "Dashboards"
sidebarMain  = a_ [href_ "#"] $ do
  "Dashdo"
  span_ [class_ "menu-icon glyphicon glyphicon-transfer"] (return ())

sidebarItem :: Monad m => (String, Text) -> HtmlT m ()
sidebarItem (fid, title) =
  a_ [href_ "#", onclick_ ("$(\".dashdo\").hide(); $(\"#" <> pack fid <>"\").show();")] $ toHtml title >>
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
            sbl = map sidebarItem (map (rdFid . snd &&& fst) ds)
            sb  = RD.mkSidebar sidebarMain sidebarTitle sbl
            sbf = RD.mkSidebarFooter (rowEven XS
              [ a_ [href_ "https://github.com/filopodia/open"] (i_ [class_ "fa fa-lg fa-github"] mempty <> "Github")
              , a_ [href_ "#"] $ i_ [id_ "spinner", class_ "fa fa-cog fa-2x"] mempty
              ])
            sbw = RD.mkSidebarWrapper sb sbf
            cw  = RD.mkPageContent (mapM_ form'_ dashdos)
            pgw = RD.mkPageWrapperOpen sbw cw
        RD.mkIndexPage (RD.mkHead "Dashdo") (RD.mkBody pgw)
        cdnJqueryJS
        cdnBootstrapJS
        script_ [src_ "/js/dashdo.js"] ("" :: Text)
