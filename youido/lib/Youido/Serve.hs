{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections,
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

import Control.Monad.Reader

--conn <-  createConn <$> readJSON "youido.json"

serveY :: a -> YouidoT (ReaderT a IO) () -> IO ()
serveY x (YouidoT sm) = do
  y <- runReaderT (execStateT sm (Youido [] "Not found!" id [] 3000)) x
  serve x y

loginPage :: Maybe (Html ()) -> Html ()
loginPage mwarn = stdHtmlPage (return ()) $ container_ $
   row_ $ div_ [class_ "col-xs-10 col-xs-offset-1 col-sm-8 col-sm-offset-2 col-md-4 col-md-offset-4"] $ loginForm "/login" mwarn

serve :: a -> Youido (ReaderT a IO) -> IO ()
serve x y@(Youido _ _ _ users port') = do
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
      let incorrect = renderText $ loginPage $ Just $
                div_ [class_ "alert alert-danger"] "Incorrect user or password"
      case lookup femail users of
        Nothing -> html incorrect
        Just passwd | passwd == fpasswd -> newSession sessions femail >> redirect "/"
                    | otherwise -> html incorrect
    matchAny (regex "/*") $ do
      let go email = do
            rq <- request
            pars <- params
            --liftIO $ print ("got request", rq)
            Response stat hdrs conts <- liftIO $ runReaderT (run y (rq, pars,email)) x
            status stat
            mapM_ (uncurry setHeader) hdrs
            raw conts
      msess <- lookupSession sessions
      case msess of
        Nothing -> if null users then go "" else redirect "/login"
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

rdashSidebar :: Text -> [((Text, Text), Text)] -> Html ()
rdashSidebar title links = do
  let mklink :: ((Text, Text),Text) -> Html ()
      mklink ((title, fa), dest) =
        a_ [href_ dest] $
          toHtml title <> i_ [class_ ("fa fa-"<>fa<>" menu-icon")] mempty
      sidebar = map mklink links
      sidebarMain  = a_ [href_ "#"] $ do
          toHtml title
          span_ [class_ "menu-icon glyphicon glyphicon-transfer"] (return ())
      sb = RD.mkSidebar sidebarMain (span_ "Dashboards") $ sidebar
      sbfoot = ""
  RD.mkSidebarWrapper sb sbfoot


mkSidebar :: [(Text, Text)] -> Html ()
mkSidebar links = ul_ $ mapM_ f links where
  f (title, dest) = li_ $ a_ [href_ dest] $: title

infixl 0 *~
(*~) :: ToURL a => b -> a -> (b, Text)
t *~ x = (t, toURL x)
