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

dashdoHandler :: (KnownSymbol s, MonadIO m) => Key s -> Dashdo t -> IO (s :/ DashdoReq -> m AsHtml)
dashdoHandler s d = do
  (iniHtml, ff) <- dashdoGenOut d (initial d)
  let dispatch (_ :/ Initial) = return $ AsHtml iniHtml
      dispatch (_ :/ Submit ffs) = do
        let newval = parseForm (initial d) ff ffs
        (thisHtml, _) <- liftIO $ dashdoGenOut d newval
        return $ AsHtml thisHtml
  return dispatch
