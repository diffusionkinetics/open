{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
             DeriveGeneric, ExtendedDefaultRules, FlexibleContexts#-}

module Youido.Serve where

import Youido.Types
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.HttpAuth

import Lucid
import Lucid.Bootstrap
import Lucid.Bootstrap3
import qualified Lucid.Rdash as RD
import Control.Concurrent.STM
import qualified Data.IntMap
import Data.Text (Text)
import Data.Monoid
import Control.Monad.State.Strict hiding (get)


import Control.Monad.IO.Class
import Control.Monad.Reader
import System.Random

type Session = ()

--conn <-  createConn <$> readJSON "youido.json"

serveY :: a -> YouidoT (ReaderT a IO) () -> IO ()
serveY x (YouidoT sm) = do
  y <- runReaderT (execStateT sm (Youido [] "Not found!" id [] 3000)) x
  serve x y

serve :: a -> Youido (ReaderT a IO) -> IO ()
serve x y@(Youido _ _ _ users port) = do
  session <- newTVarIO (Data.IntMap.empty)
  scotty port $ do
    middleware $ logStdout
    when (not $ null users) $
      middleware $ basicAuth (\u p -> case lookup u users of
                                        Nothing -> return False
                                        Just passwd -> return $ p == passwd)
          "Youidoapp"
    get "/login" $ do
      html $ renderText $ stdHtmlPage (return ()) $ container_ $ loginForm "/login" Nothing
    post "/login" $ do
      femail <- param "inputEmail"
      fpasswd <- param "inputPassword"
      let incorrect = renderText $ stdHtmlPage (return ()) $
             container_ $ loginForm "/login" $ Just "Incorrect user or password"
      case lookup femail users of
        Nothing -> html incorrect
        Just passwd | passwd == fpasswd -> redirect "/"
                    | otherwise -> html incorrect
    matchAny (regex "/*") $ do
      rq <- request
      pars <- params
      --liftIO $ print ("got request", rq)
      Response stat hdrs conts <- liftIO $ runReaderT (run y (rq, pars)) x
      status stat
      mapM_ (uncurry setHeader) hdrs
      raw conts

newSession :: TVar (Data.IntMap.IntMap ()) -> ActionM ()
newSession tv = do
  n <- liftIO $ randomRIO (0,99999999999)
  liftIO $ atomically $ modifyTVar' tv (Data.IntMap.insert n ())
  return ()



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
