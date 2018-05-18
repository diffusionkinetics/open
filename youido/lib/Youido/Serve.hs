{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections, RankNTypes,
             DeriveGeneric, ExtendedDefaultRules, FlexibleContexts#-}

module Youido.Serve where

import Youido.Types
import Youido.Authentication
import Web.Scotty
import Web.Scotty.Cookie
import Network.Wai.Middleware.RequestLogger (logStdout)

import Lucid
import Lucid.Bootstrap
import Lucid.Bootstrap3
import qualified Lucid.Rdash as RD
import Control.Concurrent.STM
import qualified Data.IntMap
import Data.Text (Text, pack, unpack)
import Data.Monoid
import Control.Monad.State.Strict hiding (get)
import qualified Data.Map.Strict as Map

import Control.Monad.Reader


serveY :: Monad m => (forall a. auth -> m a -> IO a)  -> YouidoT auth m () -> IO ()
serveY runM (YouidoT sm) = do
  let youidoDef = Youido [] "Not found!" (const id) (const $ const $const $return Nothing) 3000
  y  <- execStateT sm youidoDef
  serve runM y

loginPage :: Maybe (Html ()) -> Html ()
loginPage mwarn = stdHtmlPage (return ()) $ container_ $
   row_ $ div_ [class_ "col-sm-12 col-md-4 col-md-offset-4"] $ loginForm "/login" mwarn

serve :: Monad m => (forall a. auth -> m a -> IO a) -> Youido auth m -> IO ()
serve runM y@(Youido _ _ _ looku port') = do
  sessions <- newTVarIO (Data.IntMap.empty)
  scotty port' $ do
    middleware $ logStdout
    get "/login" $ do
      html $ renderText $ loginPage Nothing
    get "/logout" $ do
      msess <- lookupSession sessions
      case msess of
        Nothing -> return ()
        Just (i,_) -> deleteSession sessions i
      html $ renderText $ loginPage $ Just $ div_ [class_ "alert alert-info"] "Goodbye!"
    post "/login" $ do
      femail <- param "inputEmail"
      fpasswd <- param "inputPassword"
      rq <- request
      let incorrect = renderText $ loginPage $ Just $
                div_ [class_ "alert alert-danger"] "Incorrect user or password"
      mu <- liftIO $  looku rq femail fpasswd
      case mu of
        Nothing -> html incorrect
        Just u -> newSession sessions u >> redirect "/"

    matchAny (regex "/*") $ do
      let go u = do
            rq <- request
            pars <- params
            --liftIO $ print ("got request", rq)
            Response stat hdrs conts <- liftIO $ runM u $ run y u (rq, pars)
            status stat
            mapM_ (uncurry setHeader) hdrs
            raw conts
      msess <- lookupSession sessions
      case msess of
        Nothing -> redirect "/login"
        Just (i,u) -> go u


dashdoCustomJS :: Html ()
dashdoCustomJS =
  script_ "$(function(){$('#dashdoform').dashdo({uuidInterval:-1})})"

stdHtmlPage :: Html () -> Html () -> Html ()
stdHtmlPage hdrMore tbody = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    cdnCSS
    cdnThemeCSS
    cdnJqueryJS
    hdrMore
  body_ tbody

stdWrapper :: Html () -> Html () -> Html () -> Html ()
stdWrapper hdrMore sidebar h =
  stdHtmlPage hdrMore $ do
    container_ $ row_ $ do
      mkCol [(XS, 1)] $ sidebar
      mkCol [(XS, 11)] $ h

    cdnBootstrapJS
    script_ [src_ "/js/dashdo.js"] ""
    dashdoCustomJS

rdashWrapper :: Text -> Html () -> Html () -> Html () -> Html ()
rdashWrapper hdTxt hdrMore sidebar h = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    cdnCSS
    cdnFontAwesome
    cdnThemeCSS
    RD.rdashCSS
    cdnJqueryJS
    hdrMore
  body_ $ do

    let cw = RD.mkPageContent h

    RD.mkPageWrapperOpen sidebar cw
    cdnBootstrapJS
    script_ [src_ "/js/dashdo.js"] ""
    dashdoCustomJS


rdashSidebar :: Text ->  Html () -> [RD.SidebarItem] -> Html ()
rdashSidebar title sbfoot links  = do
  let sidebarMain  = a_ [href_ "#"] $ do
          toHtml title
          span_ [class_ "menu-icon glyphicon glyphicon-transfer"] (return ())
      sb = RD.mkSidebar sidebarMain links
  RD.mkSidebarWrapper sb $ RD.mkSidebarFooter sbfoot


{-mkSidebar :: [(Text, Text)] -> Html ()
mkSidebar links = ul_ $ mapM_ f links where
  f (title, dest) = li_ $ a_ [href_ dest] $: title-}

infixl 0 *~
(*~) :: ToURL a => (Text,Text) -> a -> RD.SidebarItem
(t,ic) *~ x = RD.SidebarLink t (toURL x) ic
