{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections, RankNTypes,
             DeriveGeneric, ExtendedDefaultRules, FlexibleContexts#-}

module Youido.Serve where

import           Youido.Types
import           Youido.Authentication

import           Control.Concurrent.STM
import           Lucid
import           Lucid.Bootstrap
import           Lucid.Bootstrap3
import qualified Lucid.Rdash as RD

import           Control.Monad.State.Strict hiding (get)

import           Network.HTTP.Types.URI (Query)
import           Network.HTTP.Types.Method (parseMethod, StdMethod(..))
import           Network.HTTP.Types.Status (ok200, found302, methodNotAllowed405)
import           Network.HTTP.Types.Header (Header, hContentType, hLocation, hCookie)
import           Network.Wai (queryString, Application, pathInfo, responseLBS, requestHeaders, requestMethod, Request)
import qualified Network.Wai as Wai (Response)
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Parse (parseRequestBody, lbsBackEnd)

import           Data.ByteString.Builder hiding (unpack)
import qualified Data.ByteString.Lazy as BL
import           Web.Cookie (parseCookies, renderSetCookie)

import qualified Data.IntMap
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.CaseInsensitive (mk)
import           Data.Either (either)

serveY :: Monad m => (forall a. auth -> m a -> IO a)  -> YouidoT auth m () -> IO ()
serveY runM (YouidoT sm) = do
  let youidoDef = Youido [] "Not found!" (const id) (const $ const $const $return Nothing) 3000
  y  <- execStateT sm youidoDef
  sessions <- newTVarIO Data.IntMap.empty
  let application =  logStdout $ app sessions runM y
  W.run (_port y) application

loginPage :: Maybe (Html ()) -> Html ()
loginPage mwarn = stdHtmlPage (return ()) $ container_ $
   row_ $ div_ [class_ "col-sm-12 col-md-4 col-md-offset-4"] $ loginForm "/login" mwarn

htmlContentType :: Header
htmlContentType = (hContentType, "text/html; charset=utf-8")

mkParams :: Query -> [(TL.Text, TL.Text)]
mkParams = foldr go []
  where
    go (_, Nothing) x = x
    go (k, Just v) x = (TL.fromStrict $ decodeUtf8With lenientDecode k,
                        TL.fromStrict $ decodeUtf8With lenientDecode v) : x

toWaiResponse :: Response -> Wai.Response
toWaiResponse (Response s hs c) = responseLBS s (f <$> hs) c
  where
    f :: (TL.Text, TL.Text) -> Header
    f (k, v) = (mk (BL.toStrict $ TLE.encodeUtf8 k), BL.toStrict $ TLE.encodeUtf8 v)

getParams :: Request -> IO [(TL.Text, TL.Text)]
getParams req = do
  -- TODO: Use parseRequestBodyEx to restrict resource usage
  (params, _) <- parseRequestBody lbsBackEnd req
  return ((go <$> params) ++ queryParams)
  where
    go (k, v) = (TL.fromStrict $ decodeUtf8With lenientDecode k,
                  TL.fromStrict $ decodeUtf8With lenientDecode v)
    queryParams = mkParams (queryString req)

app :: Monad m => TVar (Data.IntMap.IntMap auth) -> (forall a. auth -> m a -> IO a) -> Youido auth m -> Application
app sessions runM y@(Youido _ _ _ looku _) req sendResp = do
  let path = pathInfo req
      method = either (const Nothing) Just $ parseMethod $ requestMethod req
      query = queryString req
      headers = requestHeaders req
      cookies = maybe [] id $ (parseCookies <$> lookup hCookie headers)

  params <- getParams req

  case (method, path) of
    (Nothing, _) -> sendResp (responseLBS methodNotAllowed405 [] "")

    (Just GET, ["login"]) -> sendResp $ (responseLBS ok200 []) $ renderBS $ loginPage Nothing

    (Just GET, ["logout"]) -> do
      deleteSession sessions cookies
      sendResp $ (responseLBS ok200 [htmlContentType]) $ renderBS $ loginPage $ Just $ div_ [class_ "alert alert-info"] "Goodbye!"

    (Just POST, ["login"]) -> do
      let femail = maybe "" id (lookup "inputEmail" params)
          fpasswd = maybe "" id (lookup "inputPassword" params)
          incorrect = renderBS $ loginPage $ Just $
                      div_ [class_ "alert alert-danger"] "Incorrect user or password"

      mu <- looku req  (TL.toStrict femail) (encodeUtf8 $ TL.toStrict fpasswd)
      case mu of
        Nothing -> sendResp $ responseLBS ok200 [] incorrect
        Just u -> do
          setCookie <- BL.toStrict . toLazyByteString . renderSetCookie <$> newSession sessions u
          sendResp (responseLBS found302 [(hLocation, "/"), ("Set-Cookie", setCookie)] "")

    _ -> do
      s <- lookupSession sessions cookies
      case s of
        Nothing -> sendResp (responseLBS found302 [(hLocation, "/login")] "")
        Just u -> do
          resp <- runM u $ run y u (req, params)
          sendResp (toWaiResponse resp)


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
