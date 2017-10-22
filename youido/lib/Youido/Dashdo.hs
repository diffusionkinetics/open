{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   ExtendedDefaultRules, FlexibleContexts, TemplateHaskell,
   OverloadedLabels, TypeOperators, DataKinds, KindSignatures #-}

module Youido.Dashdo (dashdoGlobal, dashdo, DashdoReq (Initial)) where

import Youido.Types
import Youido.Serve
import Dashdo.Types hiding (FormFields)
import Dashdo.Serve
import Dashdo
import Control.Monad.IO.Class
import Network.Wai
import Data.Text.Lazy (toStrict)
import Data.Text (Text, pack, unpack)
import Data.Monoid
import qualified Data.Text.Lazy as TL
import Lucid
import GHC.OverloadedLabels
import GHC.TypeLits
import Data.Proxy
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Lucid.PreEscaped
import Lens.Micro.Platform


-- | include this once to get global JS and app UUID routes
dashdoGlobal' :: MonadIO m => m (Handler m)
dashdoGlobal' = do
  uuid <- liftIO $ getRandomUUID
  let h1 = H $ \(_ :: "uuid" :/ () ) -> return $ toStrict uuid
  let h2 = H $ \(_ :: "js" :/ "dashdo.js" :/ () ) -> return $ JS dashdoJS
  return $ h1 <> h2

-- request for a dashdo app
data DashdoReq = Initial
               | Submit [(TL.Text, TL.Text)]
               | Action Text [(TL.Text, TL.Text)]

instance FromRequest DashdoReq where
  fromRequest rq = case fromRequest rq of
    Just (Get ())-> Just Initial
    Just (Post (Left ((_ :/ Name actName (FormFields ffs) :: "action" :/ Name FormFields))))
       -> Just $ Action actName ffs
    Just (Post (Right (FormFields ffs)))
       -> Just $ Submit ffs
    Nothing -> Nothing

instance ToURL DashdoReq where
  toURL _ = "/"

dashdoHandler' :: forall s m t. (KnownSymbol s, MonadIO m, Show t) => Key s -> Dashdo m t -> m (s :/ DashdoReq -> m (MAjax (Html ())))
dashdoHandler' _ d = do
  (iniHtml, ff, acts) <- dashdoGenOut d (initial d) []
  let submitPath = pack $ "/"++(symbolVal (Proxy::Proxy s))
      wrapper :: TL.Text -> Html ()
      wrapper h = container_ $ form_ [ action_ submitPath,
                                       method_ "post", id_ "dashdoform"]
                                     $ preEscaped $ TL.toStrict h
      dispatch (_ :/ Initial) = return $ NoAjax $ wrapper iniHtml
      dispatch (_ :/ Submit ffs) = do
        let newval = parseForm (initial d) ff ffs
        (thisHtml, _, _) <- dashdoGenOut d newval []
        return $ Ajax $ wrapper thisHtml
      dispatch (_ :/ Action nm ffs) = do
        let newval = parseForm (initial d) ff ffs
        case lookup nm acts of
          Nothing -> liftIO $ putStrLn $ "Error: no such action"++ unpack nm
          Just go -> go newval
        (thisHtml, _, _) <- dashdoGenOut d newval []
        return $ Ajax $ wrapper thisHtml
  return dispatch

dashdoGlobal :: MonadIO m => YouidoT m ()
dashdoGlobal = do
  dgh <- liftY $ dashdoGlobal'
  handlers %= (dgh:)

dashdo :: forall s m t . (KnownSymbol s, MonadIO m, Show t) => Key s -> Dashdo m t -> YouidoT m ()
dashdo k dd = do
  dh <- liftY $ dashdoHandler' k dd
  handle dh