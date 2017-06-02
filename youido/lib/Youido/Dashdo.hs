{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   ExtendedDefaultRules, FlexibleContexts, TemplateHaskell,
   OverloadedLabels, TypeOperators, DataKinds, KindSignatures #-}

module Youido.Dashdo where

import Youido.Types
import Youido.Serve
import Dashdo.Types
import Dashdo.Serve
import Dashdo
import Control.Monad.IO.Class
import Network.Wai
import Data.Text.Lazy (toStrict)
import Data.Text (Text, pack)
import Data.Monoid
import qualified Data.Text.Lazy as TL
import Lucid
import GHC.OverloadedLabels
import GHC.TypeLits
import Data.Proxy
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Lucid.PreEscaped

-- | include this once to get global JS and app UUID routes
dashdoGlobal :: Monad m => IO (Handler m)
dashdoGlobal = do
  uuid <- getRandomUUID
  let h1 = H $ \(_ :: "uuid" :/ () ) -> return $ toStrict uuid
  let h2 = H $ \(_ :: "js" :/ "dashdo.js" :/ () ) -> return $ JS dashdoJS
  return $ h1 <> h2

-- request for a dashdo app
data DashdoReq = Initial | Submit [(TL.Text, TL.Text)]

instance FromRequest DashdoReq where
  fromRequest rq = case fromRequest rq of
    Just (Get ())-> Just Initial
    Just (Post (FormFields ffs)) -> Just $ Submit ffs
    Nothing -> Nothing

instance ToURL DashdoReq where
  toURL _ = ""

dashdoHandler :: forall s m t. (KnownSymbol s, MonadIO m) => Key s -> Dashdo t -> IO (s :/ DashdoReq -> m (Html ()))
dashdoHandler _ d = do
  (iniHtml, ff) <- dashdoGenOut d (initial d)
  let submitPath = pack $ "/"++(symbolVal (Proxy::Proxy s))
      wrapper :: TL.Text -> Html ()
      wrapper h = container_ $ form_ [ action_ submitPath,
                                       method_ "post", id_ "dashdoform"]
                                     $ preEscaped $ TL.toStrict h
      dispatch (_ :/ Initial) = return $ wrapper iniHtml
      dispatch (_ :/ Submit ffs) = do
        let newval = parseForm (initial d) ff ffs
        (thisHtml, _) <- liftIO $ dashdoGenOut d newval
        return $ wrapper thisHtml
  return dispatch
