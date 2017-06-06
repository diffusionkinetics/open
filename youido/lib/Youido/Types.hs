{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric, KindSignatures, DataKinds, TypeApplications, GADTs,
             FlexibleInstances, MultiParamTypeClasses, OverloadedLabels, TypeOperators  #-}

module Youido.Types where

import Network.Wai hiding (Response)
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import Data.Monoid
import GHC.TypeLits
import Data.Proxy
import Lucid
import Data.Aeson
import GHC.OverloadedLabels
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types
import Data.Void
import System.IO.Unsafe (unsafePerformIO)
import Network.Wai.Parse

-- | Repsonse from a handler - contents as bytestring, and headers
data Response = Response
  { code :: Status -- ^ http status
  , headers :: [(TL.Text, TL.Text)]
  , contents:: LBS.ByteString
  }

-- types that can be converted to a response: e.g. HTML and JSON
class ToResponse a where
  toResponse :: a -> Response
  wrapHtml :: (Html () -> Html ()) -> a -> a
  wrapHtml = flip const

instance ToResponse Response where
  toResponse = id

instance ToResponse (Html ()) where
  toResponse h = Response ok200
                          [("Content-Type", "text/html; charset=utf-8")]
                          (renderBS h)
  wrapHtml wrapper x = wrapper x

instance ToResponse Value where
  toResponse v = Response ok200
                          [("Content-Type", "application/json; charset=utf-8")]
                          (encode v)

newtype Javascript = JS { unJS :: LBS.ByteString }

instance ToResponse Javascript where
  toResponse (JS lbs) = Response ok200
                                 [("Content-Type", "application/javascript")]
                                 lbs

instance ToResponse Text where
  toResponse t = Response ok200
                          [("Content-Type", "text/plain; charset=utf-8")]
                          $ LBS.fromStrict $ T.encodeUtf8 t

instance (ToResponse a, ToResponse b) => ToResponse (Either a b) where
  toResponse (Left x) = toResponse x
  toResponse (Right y) = toResponse y

-- types that can be parsed from a request, maybe
class FromRequest a where
  fromRequest :: (Request,[(TL.Text, TL.Text)]) -> Maybe a

class ToURL a where
  toURL :: a -> Text

instance FromRequest Void where
  fromRequest _ = Nothing

-- Key, Symbol  and :/ are for subcomponents
instance (s ~ s') => IsLabel s (Key s') where --from bookkeeper
  fromLabel _ = Key

data Key (a :: Symbol) = Key

infixr 2 :/

data (s::Symbol) :/ a where
  (:/) :: Key s -> a -> s :/ a

instance (KnownSymbol s, ToURL a) => ToURL (s :/ a) where
  toURL (_ :/ a) = (pack $ symbolVal (Proxy::Proxy s)) <> "/" <> toURL a

instance (KnownSymbol s, FromRequest a) => FromRequest (s :/ a) where
  fromRequest (rq,pars) = case pathInfo rq of
      p:ps -> let sv = symbolVal (Proxy::Proxy s) in
              if p ==  pack sv
                then let newrq = rq {pathInfo = ps}
                     in fmap (Key :/) $ fromRequest (newrq,pars)
                else Nothing
      [] -> Nothing

instance ToURL () where toURL _ = ""

instance FromRequest () where
  fromRequest (rq,_) = case pathInfo rq of
    [] -> Just ()
    [""] -> Just ()
    _ -> Nothing

--split on method
data GetOrPost a b = Get a | Post b

instance (FromRequest a, FromRequest b) => FromRequest (GetOrPost a b) where
  fromRequest (rq,pars) = case requestMethod rq of
    "GET" -> fmap Get $ fromRequest (rq,pars)
    "POST" -> fmap Post $ fromRequest (rq,pars)

instance (FromRequest a, FromRequest b) => FromRequest (Either a b) where
  fromRequest rqpars = case (fromRequest rqpars, fromRequest rqpars) of
    (Just x, _) -> return $ Left x
    (Nothing, Just y) -> return $ Right y
    _ -> Nothing

newtype FormFields = FormFields [(TL.Text, TL.Text)]

instance FromRequest FormFields where
  fromRequest (_,pars) = Just $ FormFields pars


-- | handler box, parametrised on a monad
data Handler m where
  H :: (FromRequest a, ToResponse b) => (a -> m b) -> Handler m

instance Monad m => Monoid (Handler m) where
  mempty = H f where f :: Void -> m Text
                     f = absurd
  mappend (H f1) (H f2) = H $ \e -> case e of
    Left x -> fmap Left $ f1 x
    Right y -> fmap Right $ f2 y

data Youido m = Youido
  { handlers :: [Handler m] -- ^ list of handlers
  , notFoundHtml :: Html () -- ^ default, if nothing found
  , wrapper :: (Html () -> Html ()) -- ^ wrapper for Html
  }

-- | get a response from a request, given a list of handlers
run :: Monad m
    => Youido m
    -> (Request, [(TL.Text, TL.Text)]) -- ^ incoming request
    -> m Response
run (Youido [] notFound _) _ = return $ (toResponse notFound) { code = notFound404  }
run (Youido (H f : hs) notFound wrapperf) rq = do
  case fromRequest rq of
    Nothing -> run (Youido hs notFound wrapperf) rq
    Just x -> toResponse . wrapHtml wrapperf <$> f x

