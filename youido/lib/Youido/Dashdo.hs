{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   ExtendedDefaultRules, FlexibleContexts, TemplateHaskell,
   OverloadedLabels, TypeOperators, DataKinds, KindSignatures #-}

module Youido.Dashdo (dashdoGlobal, dashdo, DashdoReq (Initial, InitialWith)) where

import Youido.Types
import Dashdo.Types hiding (FormFields)
import Dashdo.Serve
import Dashdo
import Dashdo.Files
import Control.Monad.IO.Class
import Data.Text.Lazy (toStrict)
import Data.Text (Text, unpack, pack, intercalate)
import Data.Monoid
import qualified Data.Text.Lazy as TL
import Lucid
import GHC.TypeLits
import Data.Proxy
import Lucid.Bootstrap
import Lucid.PreEscaped
import Lens.Micro.Platform
import Control.Monad.Reader
import Data.IORef
import qualified Data.Set as Set
import Text.Parsec((<|>), unexpected)

-- | include this once to get global JS and app UUID routes
dashdoGlobal' :: Monad m => IO (Handler  m)
dashdoGlobal' = do
  uuid <- getRandomUUID
  let h1 = H $ \(_ :: "uuid" :/ () ) -> return $ toStrict uuid
  let h2 = H $ \(_ :: "js" :/ "dashdo.js" :/ () ) -> return $ JS dashdoJS
  return $ h1 <> h2

ffsStrict :: [(TL.Text, TL.Text)] -> [(Text, Text)]
ffsStrict ffs = map (\(k,v)-> (TL.toStrict k, TL.toStrict v)) ffs

ffsLazy :: [(Text, Text)] -> [(TL.Text, TL.Text)]
ffsLazy ffs = map (\(k,v)-> (TL.fromStrict k, TL.fromStrict v)) ffs

-- request for a dashdo app
data DashdoReq = Initial
               | InitialWith [(Text, Text)]
               | Submit [(TL.Text, TL.Text)]
               | Action Text [(TL.Text, TL.Text)]

instance Monad m => FromRequest m DashdoReq where
  requestParser = do
    res <- Just <$> requestParser <|> return Nothing
    case res of
      Just (Get (Right ())) -> return Initial
      Just (Post (Left ((_ :/ Name actName (FormFields ffs) :: "action" :/ Name FormFields))))
        -> return $ Action actName ffs
      Just (Post (Right (FormFields ffs)))
        -> return $ Submit ffs
      Just (Get (Left ((_ :/ FormFields ffs :: "with" :/ FormFields))))
        -> return $ InitialWith (ffsStrict ffs)
      Nothing -> unexpected "failure"

instance ToURL DashdoReq where
  toURL (InitialWith pars)
    = "/with?"<>(intercalate "&" $ map (\(k,v)->k<>"="<>v) pars)
  toURL _ = "/"


dashdoHandler' :: forall s m t auth. (KnownSymbol s, MonadIO m, Show t)
               => Key s -> Dashdo m t -> IO (s :/ DashdoReq ->  m (MAjax (Html ())))
dashdoHandler' _ d = do
  --(_, ff, acts) <- dashdoGenOut d (initial d) []
  ffRef <- newIORef []
  actRef <- newIORef []
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
      dispatch (_ :/ Initial) = do
        (iniHtml, ffs, acts) <- dashdoGenOut d (initial d) []
        liftIO $ incrRefs ffs acts
        return $ NoAjax $ wrapper iniHtml
      dispatch (_ :/ InitialWith ffs) = do
        ff <- liftIO $ readIORef ffRef
        let newval = parseForm (initial d) ff $ ffsLazy ffs
        (iniHtml, morefs, moreacts) <- dashdoGenOut d newval []
        liftIO $ incrRefs morefs moreacts
        return $ NoAjax $ wrapper iniHtml
      dispatch (_ :/ Submit ffs) = do
        ff <- liftIO $ readIORef ffRef
        let newval = parseForm (initial d) ff ffs
        (thisHtml, morefs, moreacts) <- dashdoGenOut d newval []
        liftIO $ incrRefs morefs moreacts
        return $ Ajax $ wrapper thisHtml
      dispatch (_ :/ Action nm ffs) = do
        acts <- liftIO $ readIORef actRef
        ff <- liftIO $ readIORef ffRef
        let newval = parseForm (initial d) ff ffs
        thisHtml <- case lookup nm acts of
                      Nothing -> do
                        liftIO $ putStrLn $ "Error: no such action"++ unpack nm
                        fmap fst3 $  dashdoGenOut d newval []
                      Just go -> do
                        ares <-  go newval
                        case ares of
                          DoNothing -> fmap fst3 $   dashdoGenOut d newval []
                          Reset -> fmap fst3 $ dashdoGenOut d (initial d) []
                          Goto url -> return $
                             "<div data-dashdo-redirect=\""<> TL.pack url <> "\"></div>"
        return $ Ajax $ wrapper thisHtml
  return dispatch

dashdoGlobal :: MonadIO m => YouidoT auth m ()
dashdoGlobal = do
  dgh <- liftIO $ dashdoGlobal'
  handlers %= (dgh:)


dashdo :: (KnownSymbol s, MonadIO m, Show t)
  => Key s -> Dashdo m t -> YouidoT auth m ()
dashdo k dd = do
  dh <- liftIO $ dashdoHandler' k dd
  handle dh
