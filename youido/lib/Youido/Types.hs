{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric, KindSignatures, DataKinds, TypeApplications, GADTs,
             FlexibleInstances, MultiParamTypeClasses, OverloadedLabels, CPP,
             TypeOperators, GeneralizedNewtypeDeriving, TemplateHaskell  #-}

module Youido.Types where

import Network.Wai hiding (Response)
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import Data.Text.Read(signed, decimal)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import Control.Monad.State.Strict
import Data.Monoid
import GHC.TypeLits
import Data.Proxy
import Lucid
import Data.Aeson hiding (defaultOptions)
import Data.List.Split (split, dropInitBlank, keepDelimsL, whenElt)
import Data.Char (toLower, isUpper)
import Data.List (intercalate, intersperse)
import Data.Foldable (traverse_)
import GHC.OverloadedLabels
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString)
import Network.HTTP.Types
import Data.Void
import Lens.Micro.Platform hiding (to)
import GHC.Generics
import Lucid.PreEscaped
import Lucid.Bootstrap
import Control.Applicative((<|>))

import Text.ParserCombinators.Parsec       (optionMaybe, getState)
import Text.ParserCombinators.Parsec.Pos   (incSourceLine)
import Text.ParserCombinators.Parsec.Prim  (unexpected, runParser, GenParser, getPosition, token, (<?>),
                                            many)

import Text.Digestive.View (View(..))
import qualified Text.Digestive as D
import qualified Text.Digestive.Form.List as D

import Control.Monad.Identity (Identity, runIdentity)

import Data.Maybe (maybeToList)

import qualified Text.Digestive.Lucid.Html5 as DL

--------------------------------------------------------------------------
---                 PATHINFO
--- Code copied from the web-routes library
--------------------------------------------------------------------------

type FormPars = [(TL.Text, TL.Text)]

type URLParser a = GenParser Text FormPars a

pToken :: tok -> (Text -> Maybe a) -> URLParser a
pToken _ f = do pos <- getPosition
                token unpack (const $ incSourceLine pos 1) f

-- | match on a specific string
segment :: Text -> URLParser Text
segment x = (pToken (const x) (\y -> if x == y then Just x else Nothing)) <?> unpack x

-- | match on any string
anySegment :: URLParser Text
anySegment = pToken (const "any string") Just

hyphenate :: String -> Text
hyphenate =
    pack . intercalate "-" . map (map toLower) . split splitter
  where
    splitter = dropInitBlank . keepDelimsL . whenElt $ isUpper

class GRequestInfo f where
  gtoPathSegments   :: f url -> [Text]
  gfromRequest :: URLParser (f url)

instance GRequestInfo U1 where
  gtoPathSegments U1 = []
  gfromRequest = pure U1

instance GRequestInfo a => GRequestInfo (D1 c a) where
  gtoPathSegments = gtoPathSegments . unM1
  gfromRequest = M1 <$> gfromRequest

instance GRequestInfo a => GRequestInfo (S1 c a) where
  gtoPathSegments = gtoPathSegments . unM1
  gfromRequest = M1 <$> gfromRequest

instance forall c a. (GRequestInfo a, Constructor c) => GRequestInfo (C1 c a) where
  gtoPathSegments m@(M1 x) = (hyphenate . conName) m : gtoPathSegments x
  gfromRequest = M1 <$ segment (hyphenate . conName $ (undefined :: C1 c a r))
                         <*> gfromRequest

instance (GRequestInfo a, GRequestInfo b) => GRequestInfo (a :*: b) where
  gtoPathSegments (a :*: b) = gtoPathSegments a ++ gtoPathSegments b
  gfromRequest = (:*:) <$> gfromRequest <*> gfromRequest

instance (GRequestInfo a, GRequestInfo b) => GRequestInfo (a :+: b) where
  gtoPathSegments (L1 x) = gtoPathSegments x
  gtoPathSegments (R1 x) = gtoPathSegments x
  gfromRequest = L1 <$> gfromRequest
                  <|> R1 <$> gfromRequest

instance RequestInfo a => GRequestInfo (K1 i a) where
  gtoPathSegments = toPathSegments . unK1
  gfromRequest = K1 <$> fromReq

-- This is an intermediate class between GRequestinfo and FromRequest/ToURL.
-- This is not part of the API
class RequestInfo url where
  toPathSegments :: url -> [Text]
  fromReq :: URLParser url

  default toPathSegments :: (Generic url, GRequestInfo (Rep url)) => url -> [Text]
  toPathSegments = gtoPathSegments . from
  default fromReq :: (Generic url, GRequestInfo (Rep url)) => URLParser url
  fromReq = to <$> gfromRequest

-- it's instances all the way down

instance (FromForm a) => RequestInfo (Form a) where
  toPathSegments _ = []
  fromReq = do
    result <- formResult
    case result of
        (_, Just x) -> return $ Form x
        (view, Nothing) -> return $ FormError view
    where
      formResult :: URLParser (View Text, Maybe a)
      formResult = runPostForm <$> getState

      lookupPath :: [(TL.Text, TL.Text)] -> D.Path -> [D.FormInput]
      lookupPath pars path = maybeToList $ D.TextInput <$> TL.toStrict <$> lookup (TL.fromStrict (D.fromPath path)) pars

      runPostForm :: [(TL.Text, TL.Text)] -> (View Text, Maybe a)
      runPostForm pars = runIdentity $ D.postForm "top-level-form" form (postFormHandler pars)
        where
          postFormHandler :: (Monad m) => [(TL.Text, TL.Text)] -> D.FormEncType -> m (D.Env m)
          postFormHandler pars D.UrlEncoded = return $ \path -> (return $ lookupPath pars path)
          postFormHandler pars D.MultiPart = return $ const (return [])

          form :: D.Form Text Identity a
          form = fromForm Nothing

instance (FromForm a) => RequestInfo (QueryString a) where
  toPathSegments (QueryStringLink) = []
  toPathSegments (QueryString x) = [] --TODO
  fromReq = do
   res <- formToQueryString <$> fromReq
   case res of
     Nothing -> unexpected "failed"
     Just t -> return t

formToQueryString :: FromForm a => Form a -> Maybe (QueryString a)
formToQueryString FormLink = Just $ QueryStringLink
fromToQueryString (Form a) = Just $ QueryString a
fromToQueryString _        = Nothing

instance (RequestInfo a, RequestInfo b) => RequestInfo (a,b) where
  toPathSegments (a,b)  = toPathSegments a ++ toPathSegments b
  fromReq = (,) <$> fromReq <*> fromReq

instance RequestInfo a => RequestInfo (Maybe a) where
  toPathSegments Nothing  = []
  toPathSegments (Just t) = toPathSegments t
  fromReq = optionMaybe (fromReq)

instance RequestInfo Text where
  toPathSegments = (:[])
  fromReq = anySegment

instance RequestInfo [Text] where
  toPathSegments = id
  fromReq = many anySegment

instance RequestInfo String where
  toPathSegments = (:[]) . pack
  fromReq = unpack <$> anySegment

instance RequestInfo [String] where
  toPathSegments = id . map pack
  fromReq = many (unpack <$> anySegment)

instance RequestInfo Int where
  toPathSegments i = [pack $ show i]
  fromReq = pToken (const "Int") checkIntegral

instance RequestInfo Integer where
  toPathSegments i = [pack $ show i]
  fromReq = pToken (const "Integer") checkIntegral

checkIntegral :: Integral a => Text -> Maybe a
checkIntegral txt =
  case signed decimal txt of
    (Left _) -> Nothing
    (Right (n, r))
       | T.null r -> Just n
       | otherwise -> Nothing

--------------------------------------------------------------------------
---                 RESPONSES
--------------------------------------------------------------------------


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

data MAjax a = Ajax a | NoAjax a
unMAjax :: MAjax a -> a
unMAjax (Ajax x) = x
unMAjax (NoAjax x) = x

instance ToResponse (MAjax (Html ())) where
  toResponse (h) = Response ok200
                          [("Content-Type", "text/html; charset=utf-8")]
                          (renderBS $ unMAjax h)
  wrapHtml _ (Ajax x) = Ajax x
  wrapHtml wrapper (NoAjax x) = NoAjax $ wrapper x

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

newtype AsHtml = AsHtml LBS.ByteString

instance ToResponse AsHtml where
  toResponse (AsHtml t)
    = Response ok200
         [("Content-Type", "text/html; charset=utf-8")]
         $ t
  wrapHtml wrapper (AsHtml x) = AsHtml $ renderBS $ wrapper $ preEscapedByteString x
--------------------------------------------------------------------------
---                 REQUESTS
--------------------------------------------------------------------------


-- types that can be parsed from a request, maybe
class FromRequest a where
  fromRequest :: (Request,[(TL.Text, TL.Text)]) -> Maybe a

  default fromRequest :: (Generic a, GRequestInfo(Rep a)) => (Request,[(TL.Text, TL.Text)]) -> Maybe a
  fromRequest (rq,pars) =
    let parser = to <$> gfromRequest
    in case (runParser parser pars "" (pathInfo rq)) of
         Left _ -> Nothing
         Right t -> Just t

class ToURL a where
  toURL :: a -> Text

  default toURL :: (Generic a, GRequestInfo(Rep a)) => a -> Text
  toURL a = let pathSegments = gtoPathSegments (from a) in
    T.intercalate "/" $ ("" : pathSegments)
  -- Trick to get a leading "/"

instance FromRequest Void where
  fromRequest _ = Nothing

-- Key, Symbol  and :/ are for subcomponents
instance (s ~ s') => IsLabel s (Key s') where --from bookkeeper
#if MIN_VERSION_base(4,10,0)
  fromLabel = Key
#else
  fromLabel _ = Key
#endif

data Key (a :: Symbol) = Key

infixr 2 :/

data (s::Symbol) :/ a where
  (:/) :: Key s -> a -> s :/ a

instance (KnownSymbol s, ToURL a) => ToURL (s :/ a) where
  toURL (_ :/ a) = "/" <> (pack $ symbolVal (Proxy::Proxy s)) <> toURL a

instance (KnownSymbol s, FromRequest a) => FromRequest (s :/ a) where
  fromRequest (rq,pars) = case pathInfo rq of
      p:ps -> let sv = symbolVal (Proxy::Proxy s) in
              if p ==  pack sv
                then let newrq = rq {pathInfo = ps}
                     in fmap (Key :/) $ fromRequest (newrq,pars)
                else Nothing
      [] -> Nothing

instance ToURL () where toURL _ = "/"

instance FromRequest () where
  fromRequest (rq,_) = case pathInfo rq of
    [] -> Just ()
    [""] -> Just ()
    _ -> Nothing

--capture any name
data Name a = Name Text a

instance FromRequest a => FromRequest (Name a) where
  fromRequest (rq,pars) = case pathInfo rq of
      p:ps -> let newrq = rq {pathInfo = ps}
              in fmap (Name p) $ fromRequest (newrq,pars)

      [] -> Nothing

instance ToURL a=> ToURL (Name a)
  where toURL (Name nm x) = "/"<> nm <> toURL x

--split on method
data GetOrPost a b = Get a | Post b

instance (FromRequest a, FromRequest b) => FromRequest (GetOrPost a b) where
  fromRequest (rq,pars) = case requestMethod rq of
    "GET" -> fmap Get $ fromRequest (rq,pars)
    "POST" -> fmap Post $ fromRequest (rq,pars)

onlyPOST :: Request -> Maybe a -> Maybe a
onlyPOST rq mv = if requestMethod rq == "POST" then mv else Nothing

instance (FromRequest a, FromRequest b) => FromRequest (Either a b) where
  fromRequest rqpars = case (fromRequest rqpars, fromRequest rqpars) of
    (Just x, _) -> return $ Left x
    (Nothing, Just y) -> return $ Right y
    _ -> Nothing

newtype FormFields = FormFields [(TL.Text, TL.Text)]

instance FromRequest FormFields where
  fromRequest (_,pars) = Just $ FormFields pars

instance FromRequest a => FromRequest (a, Request) where
  fromRequest (rq, pars) = (,) <$> fromRequest (rq, pars) <*> (Just rq)


instance RequestInfo FormFields where
  toPathSegments _ = []
  fromReq = do
    result <- getState
    return $ FormFields result

--------------------------------------------------------------------------
---                 FORM HANDLING
--------------------------------------------------------------------------

data Options = Options
  {
    fieldLabelModifier :: Text -> Text
  , constructorTagModifier :: Text -> Text
  }

defaultOptions :: Options
defaultOptions = Options id id

genericFromForm :: (Generic a, PostFormG (Rep a), Monad m) => D.Formlet Text m a
genericFromForm def = to <$>  postFormG (from  <$> def)

genericRenderForm :: (Generic a, PostFormG (Rep a), Monad m) => Proxy a -> Options -> View Text -> HtmlT m ()
genericRenderForm p options view =  do
  DL.errorList "" (toHtml <$> view)
  renderFormG (from <$> p) options view

class FromForm a where
  fromForm ::
    (Monad m) => D.Formlet Text m a
  default fromForm ::
    (Generic a, PostFormG (Rep a), Monad m) => D.Formlet Text m a
  fromForm def = to <$>  postFormG (from  <$> def)

  renderForm :: Monad m => Proxy a  -> View Text -> HtmlT m ()
  default renderForm :: (Generic a, PostFormG (Rep a), Monad m) => Proxy a  -> View Text -> HtmlT m ()
  renderForm p = genericRenderForm p defaultOptions

  getView :: Maybe a -> View Text
  getView def = runIdentity $ D.getForm "top-level-form" $ fromForm def

type Label = Text
type FieldName = Text
type InputType = Text

renderBootstrapInput :: (Monad m) => InputType -> [Attribute]
                     -> FieldName -> Label
                     -> View Text -> HtmlT m ()
renderBootstrapInput typ_ attrs fieldName label view = div_ [class_ "form-group"] $ do
    DL.label fieldName view (toHtml label)
    with (DL.inputWithType typ_ attrs fieldName view)
      [class_ "form-control", autofocus_]
    DL.errorList fieldName (toHtml <$> view)

class FormField a where
  fromFormField :: (Monad m) => D.Formlet Text m a

  renderField :: (Monad m) => Proxy a -> Text -> Text -> View Text -> HtmlT m ()
  renderField _ = renderBootstrapInput "text" []
---------------------------------------------------------------------------------

class PostFormG f where
  postFormG :: Monad m => D.Formlet Text m (f a)
  renderFormG :: Monad m => Proxy (f a) -> Options -> View Text -> HtmlT m ()

instance PostFormG f => PostFormG (M1 D t f) where
  postFormG def = M1 <$> (postFormG $ unM1 <$> def)
  renderFormG _ = renderFormG (Proxy :: Proxy (f a))

instance (PostFormG f) => PostFormG (M1 C t f) where
  postFormG def = M1 <$> (postFormG $ unM1 <$> def)
  renderFormG _ = renderFormG (Proxy :: Proxy (f a))

instance (Selector t, FormField a) => PostFormG (M1 S t (K1 i a)) where
  postFormG def = M1 .K1 <$> (fieldName D..:(fromFormField $ (unK1 . unM1  <$> def)))
   where
     val :: M1 S t (K1 i a) r
     val = undefined

     fieldName :: Text
     fieldName = T.pack $ selName val

  renderFormG _ options view = renderField (Proxy :: Proxy a) fieldName (fieldLabelModifier options $ fieldName) view
   where
     val :: M1 S t (K1 i a) r
     val = undefined

     fieldName :: Text
     fieldName = T.pack $ selName val

instance (PostFormG f, PostFormG g) => PostFormG (f :*: g) where
  postFormG (Just (def1 :*: def2)) = (:*:) <$> (postFormG $ Just def1) <*> (postFormG $ Just def2)
  postFormG Nothing = (:*:) <$> (postFormG Nothing) <*> (postFormG Nothing)

  renderFormG _ options view = do
    renderFormG (Proxy :: Proxy (f a)) options view
    renderFormG (Proxy :: Proxy (g a)) options view

instance FormField Text where
  fromFormField = D.text

instance FormField Bool where
  renderField _ fieldName label view = div_ [class_ "checkbox"] $ do
    DL.label fieldName view $ do
      with (DL.inputCheckbox fieldName (toHtml <$> view))
        [autofocus_]
      toHtml label
    DL.errorList fieldName (toHtml <$> view)

  fromFormField = D.bool

instance FormField Int where
  fromFormField = D.stringRead "must be an integer"

instance FormField Double where
  fromFormField = D.stringRead "must be a double"

  renderField _ =renderBootstrapInput "number" []

renderItem :: forall m a. (FromForm a, Monad m)
  => Proxy a -> Text -> Int -> Text -> View Text -> HtmlT m ()
renderItem _ fieldName idx context v = do
  let jsArgs = "(this.parentNode,'" <> fieldName <> "','" <> context
               <> "'); return false;"

  div_ [class_ "youido_multi_item well container"] $ do
    renderForm (Proxy :: Proxy a) v
    fieldButton "Delete" $ "youidoRemoveItem" <> jsArgs
    span_ [] $ " | "
    fieldButton "Add new item" $ "youidoAddItem" <> jsArgs

fieldButton :: Monad m => Text -> Text -> HtmlT m ()
fieldButton name onclick =
  button_ [ type_ "button", onclick_ $ onclick ] $ do
    a_ [] (toHtml name)

instance FromForm a => FormField [a] where
  fromFormField = D.listOf fromForm
  renderField _ fieldName label view = do
    let fieldPath = D.absolutePath fieldName view
        fieldPathT = D.fromPath fieldPath
        indicesPath = fieldPath ++ [D.indicesRef]
        indicesPathT = D.fromPath indicesPath
        indicesPathRel = D.fromPath [fieldName, D.indicesRef]

        items = D.listSubViews fieldName view
        addHidden = if length items == 0 then "" else "display: none"
        onclick = "youidoAddLoneItem(this.parentNode, '"
                  <> fieldName <> "', '" <> fieldPathT
                  <> "'); return false;"
    DL.label fieldName view $ toHtml label
    div_ [class_ "youido_multi_list form-group"] $ do

      -- Invisible input holds the "indices" path with a value
      input_ [ style_ "display: none"
             , id_ indicesPathT
             , name_ indicesPathT
             , value_ (D.fieldInputText indicesPathRel view)]

      -- Invisible item so that the JS knows how to render
      -- a form when the list is empty
      let
        dummyView = D.makeListSubView fieldName (-1) view
        dummy = renderItem (Proxy :: Proxy a)
                fieldName (-1) fieldPathT dummyView
      with dummy [ style_ "display: none"
                 , id_ (fieldPathT <> ".youido_dummy_item")]
      with (fieldButton "Add new item" onclick)
        [style_ addHidden, id_ (fieldPathT <> ".youido_add_lone_item")]

      traverse_ (\(v,idx) ->
                   let ctx = D.fromPath $ viewName v : init (viewContext v)
                   in renderItem (Proxy :: Proxy a) fieldName idx ctx v)
        $ zip items [0..]
      DL.errorList fieldName (toHtml <$> view)

enumFieldFormlet :: (Enum a, Bounded a, Eq a, Monad m, Show a) => D.Formlet Text m a
enumFieldFormlet = D.choice (map (\x -> (x, T.pack . show $ x)) [minBound..maxBound])

-- when a field is wrapped in a Form type, switch to getting the
-- data using FromForm when deriving FromRequest

data Form a = FormLink | FormError (View Text) | Form a deriving (Show, Generic)
data QueryString a = QueryStringLink | QueryString a deriving (Show, Generic)

--------------------------------------------------------------------------
---                 HANDLERS
--------------------------------------------------------------------------

type Email = Text

-- | handler box, parametrised on a monad
data Handler m where
  H :: (FromRequest a, ToResponse b) => (a -> m b) -> Handler m

instance Monad m => Monoid (Handler m) where
  mempty = H f where f :: Void -> m Text
                     f = absurd
  mappend (H f1) (H f2) = H $ \e -> case e of
    Left x -> fmap Left $ f1 x
    Right y -> fmap Right $ f2 y

data Youido auth m = Youido
  { _handlers :: [Handler m] -- ^ list of handlers
  , _notFoundHtml :: Html () -- ^ default, if nothing found
  , _wrapper :: auth -> (Html () -> Html ()) -- ^ wrapper for Html
  , _lookupUser:: Request -> Email-> ByteString-> IO (Maybe auth)
  , _port :: Int
  }

makeLenses ''Youido

newtype YouidoT auth m a = YouidoT {unYouidoT :: StateT (Youido auth m) IO a}
   deriving (Functor, Applicative, Monad, MonadIO, MonadState (Youido auth m))

handle :: (FromRequest a, ToResponse b, Monad m)
       => (a -> m b) -> YouidoT auth m ()
handle f = handlers %= ((H f):)

unHtmlT :: Monad m => (a -> HtmlT m ()) ->(a -> m AsHtml)
unHtmlT f x = fmap AsHtml $ renderBST $ f x

hHtmlT :: (FromRequest a, Monad m)
       => (a -> HtmlT m ()) -> YouidoT auth m ()
hHtmlT f = handlers %= ((H $ unHtmlT f):) where

-- | get a response from a request, given a list of handlers
run :: Monad m
    => Youido auth m
    -> auth
    -> (Request, [(TL.Text, TL.Text)]) -- ^ incoming request
    -> m Response
run (Youido [] notFound wrapperf _ _) u _ =
  return $
    (toResponse $ wrapHtml (wrapperf u) notFound)
      { code = notFound404  }
run (Youido (H f : hs) notFound wrapperf lu p) u rq = do
  case fromRequest rq of
    Nothing -> run (Youido hs notFound wrapperf lu p) u rq
    Just x -> toResponse . wrapHtml (wrapperf u) <$>  f x
