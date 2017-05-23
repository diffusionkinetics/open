{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric, KindSignatures, DataKinds, TypeApplications, GADTs,
             FlexibleInstances, MultiParamTypeClasses, OverloadedLabels, TypeOperators  #-}

module Youido.Types where

import Network.Wai hiding (Response)
import Data.Text (Text, pack)
import Data.Monoid
import GHC.TypeLits
import Data.Proxy
import Lucid
import Data.Aeson
import GHC.OverloadedLabels
import qualified Data.ByteString.Lazy as LBS

data Response = Response { code :: Int, headers :: [(Text, Text)], contents:: LBS.ByteString}

class ToResponse a where
  toResponse :: a -> Response

instance ToResponse Response where
  toResponse = id

instance ToResponse (Html ()) where
  toResponse h = Response 200 [("Content-Type", "text/html; charset=utf-8")] (renderBS h)

instance ToResponse Value where
  toResponse v = Response 200 [("Content-Type", "application/json; charset=utf-8")] (encode v)

class FromRequest a where
  fromRequest :: Request -> Maybe a

class ToURL a where
  toURL :: a -> Text


instance (s ~ s') => IsLabel s (Key s') where --from bookkeeper
  fromLabel _ = Key

data Key (a :: Symbol) = Key

data (s::Symbol) :/ a where
  (:/) :: Key s -> a -> s :/ a

instance (KnownSymbol s, ToURL a) => ToURL (s :/ a) where
  toURL (_ :/ a) = (pack $ symbolVal (Proxy::Proxy s)) <> "/" <> toURL a

instance (KnownSymbol s, FromRequest a) => FromRequest (s :/ a) where
  fromRequest rq = case pathInfo rq of
      p:ps -> if p == pack (symbolVal (Proxy::Proxy s))
                then fromRequest rq {pathInfo = ps}
                else Nothing
      [] -> Nothing

instance ToURL () where toURL _ = ""

instance FromRequest () where
  fromRequest rq = case pathInfo rq of
    [] -> Just ()
    _ -> Nothing

--s :: Text
--s = toURL (#foo :/ ())

data Handler m where
  H :: (FromRequest a, ToResponse b) => (a -> m b) -> Handler m

run :: Monad m => [Handler m] -> Request -> Html () -> m Response
run [] _ notFound = return $ (toResponse notFound) { code = 404 }
run (H f : hs) rq notFound = do
  case fromRequest rq of
    Nothing -> run hs rq notFound
    Just x -> toResponse <$> f x

