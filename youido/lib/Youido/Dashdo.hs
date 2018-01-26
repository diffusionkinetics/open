{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   ExtendedDefaultRules, FlexibleContexts, TemplateHaskell,
   OverloadedLabels, TypeOperators, DataKinds, KindSignatures #-}

module Youido.Dashdo (dashdoGlobal, dashdo, DashdoReq (DashdoLink, InitialWith)) where

import Youido.Types
import Youido.Serve
import Dashdo.Types hiding (FormFields)
import Dashdo.Serve
import Dashdo
import Dashdo.Files
import Control.Monad.IO.Class
import Network.Wai
import Data.Text.Lazy (toStrict)
import Data.Text (Text, pack, unpack, intercalate)
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
import Control.Monad.Reader
import Data.IORef
import qualified Data.Set as Set


-- | include this once to get global JS and app UUID routes
dashdoGlobal' :: MonadIO m => m (Handler m)
dashdoGlobal' = do
  uuid <- liftIO $ getRandomUUID
  let h1 = H $ \(_ :: "uuid" :/ () ) -> return $ toStrict uuid
  let h2 = H $ \(_ :: "js" :/ "dashdo.js" :/ () ) -> return $ JS dashdoJS
  return $ h1 <> h2

ffsStrict :: [(TL.Text, TL.Text)] -> [(Text, Text)]
ffsStrict ffs = map (\(k,v)-> (TL.toStrict k, TL.toStrict v)) ffs

ffsLazy :: [(Text, Text)] -> [(TL.Text, TL.Text)]
ffsLazy ffs = map (\(k,v)-> (TL.fromStrict k, TL.fromStrict v)) ffs

-- request for a dashdo app
data DashdoReq = Initial User
               | DashdoLink
               | InitialWith [(Text, Text)] User
               | Submit [(TL.Text, TL.Text)] User
               | Action Text [(TL.Text, TL.Text)] User

instance FromRequest DashdoReq where
  fromRequest rq@(_,_,u) = case fromRequest rq of
    Just (Get (Right ()))-> Just $ Initial u
    Just (Post (Left ((_ :/ Name actName (FormFields ffs) :: "action" :/ Name FormFields))))
       -> Just $ Action actName ffs u
    Just (Post (Right (FormFields ffs)))
       -> Just $ Submit ffs u
    Just (Get (Left ((_ :/ FormFields ffs :: "with" :/ FormFields))))
       -> Just $ InitialWith (ffsStrict ffs) u
    Nothing -> Nothing

instance ToURL DashdoReq where
  toURL (InitialWith pars _)
    = "/with?"<>(intercalate "&" $ map (\(k,v)->k<>"="<>v) pars)
  toURL _ = "/"

dashdoHandler' :: forall s m t. (KnownSymbol s, MonadIO m, Show t)
               => Key s -> Dashdo (ReaderT User m) t -> m (s :/ DashdoReq -> m (MAjax (Html ())))
dashdoHandler' _ d = do
  --(_, ff, acts) <- dashdoGenOut d (initial d) []
  ffRef <- liftIO $ newIORef []
  actRef <- liftIO $ newIORef []
  let fst3 (x,_,_) = x
      incrRefs theseFF theseActs = do
        ff <- readIORef ffRef
        let currentKeys = Set.fromList $ map fst ff
        let newFields = filter (\(k,_)->Set.notMember k currentKeys) theseFF
        when (not $ null newFields) $
          modifyIORef ffRef (++newFields)
        acts <- readIORef actRef
        let currentKeys' = Set.fromList $ map fst acts
        let newFields' = filter (\(k,_)->Set.notMember k currentKeys') theseActs
        when (not $ null newFields') $
          modifyIORef actRef (++newFields')
      submitPath = pack $ "/"++(symbolVal (Proxy::Proxy s))
      wrapper :: TL.Text -> Html ()
      wrapper h = container_ $ form_ [ action_ submitPath,
                                       method_ "post", id_ "dashdoform"]
                                     $ preEscaped $ TL.toStrict h
      dispatch (_ :/ Initial u) = do
        (iniHtml, ffs, acts) <- flip runReaderT u $ dashdoGenOut d (initial d) []
        liftIO $ incrRefs ffs acts
        return $ NoAjax $ wrapper iniHtml
      dispatch (_ :/ InitialWith ffs u) = do
        ff <- liftIO $ readIORef ffRef
        let newval = parseForm (initial d) ff $ ffsLazy ffs
        (iniHtml, morefs, moreacts) <- flip runReaderT u $ dashdoGenOut d newval []
        liftIO $ incrRefs morefs moreacts
        return $ NoAjax $ wrapper iniHtml
      dispatch (_ :/ Submit ffs u) = do
        ff <- liftIO $ readIORef ffRef
        let newval = parseForm (initial d) ff ffs
        (thisHtml, morefs, moreacts) <- flip runReaderT u $ dashdoGenOut d newval []
        liftIO $ incrRefs morefs moreacts
        return $ Ajax $ wrapper thisHtml
      dispatch (_ :/ Action nm ffs u) = do
        acts <- liftIO $ readIORef actRef
        ff <- liftIO $ readIORef ffRef
        let newval = parseForm (initial d) ff ffs
        thisHtml <- case lookup nm acts of
                      Nothing -> do
                        liftIO $ putStrLn $ "Error: no such action"++ unpack nm
                        fmap fst3 $ flip runReaderT u $ dashdoGenOut d newval []
                      Just go -> do
                        ares <- flip runReaderT u $ go newval
                        case ares of
                          DoNothing -> fmap fst3 $  flip runReaderT u $ dashdoGenOut d newval []
                          Reset -> fmap fst3 $ flip runReaderT u $ dashdoGenOut d (initial d) []
                          Goto url -> return $
                             "<div data-dashdo-redirect=\""<> TL.pack url <> "\"></div>"
        return $ Ajax $ wrapper thisHtml
  return dispatch

dashdoGlobal :: MonadIO m => YouidoT m ()
dashdoGlobal = do
  dgh <- liftY $ dashdoGlobal'
  handlers %= (dgh:)

dashdo :: forall s m t . (KnownSymbol s, MonadIO m, Show t) => Key s -> Dashdo (ReaderT User m) t -> YouidoT m ()
dashdo k dd = do
  dh <- liftY $ dashdoHandler' k dd
  handle dh