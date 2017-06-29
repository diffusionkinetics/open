{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
             DeriveGeneric, ExtendedDefaultRules, FlexibleContexts#-}

module Youido.Serve where

import Youido.Database
import Youido.Types
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.HttpAuth

import Lucid
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Data.Text (Text)


import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import Control.Monad.Reader

type Session = ()

--conn <-  createConn <$> readJSON "youido.json"

serve :: a -> Youido (ReaderT a IO) -> IO ()
serve x y@(Youido _ _ _ users port) = do
  scotty port $ do
   middleware $ logStdout
   when (not $ null users) $
     middleware $ basicAuth (\u p -> case lookup u users of
                                       Nothing -> return False
                                       Just passwd -> return $ p == passwd)
        "Youidoapp"
   matchAny (regex "/*") $ do
     rq <- request
     pars <- params
     --liftIO $ print ("got request", rq)
     Response stat hdrs conts <- liftIO $ runReaderT (run y (rq, pars)) x
     status stat
     mapM_ (uncurry setHeader) hdrs
     raw conts

stdWrapper :: Html () -> Html () -> Html () -> Html ()
stdWrapper hdrMore sidebar h = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    cdnCSS
    cdnThemeCSS
    hdrMore

  body_ $ do
    container_ $ row_ $ do
      mkCol [(XS, 1)] $ sidebar
      mkCol [(XS, 11)] $ h

    cdnJqueryJS
    cdnBootstrapJS
    script_ [src_ "/js/dashdo.js"] ""
    script_ [src_ "/js/runners/youido.js"] ""

mkSidebar :: [(Text, Text)] -> Html ()
mkSidebar links = ul_ $ mapM_ f links where
  f (title, dest) = li_ $ a_ [href_ dest] $: title

infixl 0 *~
(*~) :: ToURL a => Text -> a -> (Text, Text)
t *~ x = (t, toURL x)
