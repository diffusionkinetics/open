{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
import Data.List (intercalate)
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

import Text.Parsec       (optionMaybe, getState, putState)
import Text.ParserCombinators.Parsec.Pos   (incSourceLine)
import Text.Parsec (ParsecT, runParserT, tokenPrim)
import Text.ParserCombinators.Parsec.Prim  (unexpected, GenParser, getPosition, (<?>),
                                            many)

import Text.Digestive.View (View(..))
import qualified Text.Digestive as D
import qualified Text.Digestive.Form.List as D

import Control.Monad.Identity (Identity, runIdentity)

import Data.Maybe (maybeToList)

import qualified Text.Digestive.Lucid.Html5 as DL

import Control.Monad.Trans.Class

--------------------------------------------------------------------------
---                 PATHINFO
--- Code copied from the web-routes library
--------------------------------------------------------------------------

type FormPars = [(TL.Text, TL.Text)]

type URLParser m a = ParsecT [Text] (Request, FormPars) m a

pToken :: (Monad m) => tok -> (Text -> Maybe a) -> URLParser m a
pToken _ f = do pos <- getPosition
                tokenPrim unpack (\pos _ _ -> incSourceLine pos 1) f

-- | match on a specific string
segment :: Monad m => Text -> URLParser m Text
segment x = (pToken (const x) (\y -> if x == y then Just x else Nothing)) <?> unpack x

-- | match on any string
anySegment :: Monad m => URLParser m Text
anySegment = pToken (const "any string") Just

hyphenate :: String -> Text
hyphenate =
    pack . intercalate "-" . map (map toLower) . split splitter
  where
    splitter = dropInitBlank . keepDelimsL . whenElt $ isUpper

class GToURL f where
  gtoURL   :: f url -> Text

instance GToURL U1 where
  gtoURL U1 = ""

instance GToURL a => GToURL (D1 c a) where
  gtoURL = gtoURL . unM1

instance GToURL a => GToURL (S1 c a) where
  gtoURL = gtoURL . unM1

instance forall c a. (GToURL a, Constructor c) => GToURL (C1 c a) where
  gtoURL m@(M1 x) = (hyphenate . conName) m <> "/" <> gtoURL x

instance (GToURL a, GToURL b) => GToURL (a :*: b) where
  gtoURL (a :*: b) = gtoURL a <> gtoURL b

instance (GToURL a, GToURL b) => GToURL (a :+: b) where
  gtoURL (L1 x) = gtoURL x
  gtoURL (R1 x) = gtoURL x

instance ToURL a => GToURL (K1 i a) where
  gtoURL = toURL . unK1

class GFromRequest m f where
  grequestParser :: URLParser m (f url)

instance GFromRequest m U1 where
  grequestParser = pure U1

instance GFromRequest m a => GFromRequest m (D1 c a) where
  grequestParser = M1 <$> grequestParser

instance GFromRequest m a => GFromRequest m (S1 c a) where
  grequestParser = M1 <$> grequestParser

instance forall c a m. (Monad m, GFromRequest m a, Constructor c) => GFromRequest m (C1 c a) where
  grequestParser = M1 <$ segment (hyphenate . conName $ (undefined :: C1 c a r))
                         <*> grequestParser

instance (GFromRequest m a, GFromRequest m b) => GFromRequest m (a :*: b) where
  grequestParser = (:*:) <$> grequestParser <*> grequestParser

instance (GFromRequest m a, GFromRequest m b) => GFromRequest m (a :+: b) where
  grequestParser = L1 <$> grequestParser
                  <|> R1 <$> grequestParser

instance FromRequest m a => GFromRequest m (K1 i a) where
  grequestParser = K1 <$> requestParser

-- it's instances all the way down
instance ToURL (Form a) where
  toURL _ = ""

instance (Monad m, FromForm m a) => FromRequest m (Form a) where
  requestParser = do
    result <- formResult
    return $ case result of
        (_, Just x) -> Form x
        (view, Nothing) -> FormError view
    where
      formResult :: URLParser m (View Text, Maybe a)
      formResult = do
        (_, pars) <- getState
        lift $ runPostForm pars

      lookupPath :: [(TL.Text, TL.Text)] -> D.Path -> [D.FormInput]
      lookupPath pars path = maybeToList $ D.TextInput <$> TL.toStrict <$> lookup (TL.fromStrict (D.fromPath path)) pars

      runPostForm :: [(TL.Text, TL.Text)] -> m (View Text, Maybe a)
      runPostForm pars = D.postForm "top-level-form" form (postFormHandler pars)
        where
          postFormHandler :: (Monad m) => [(TL.Text, TL.Text)] -> D.FormEncType -> m (D.Env m)
          postFormHandler pars D.UrlEncoded = return $ \path -> (return $ lookupPath pars path)
          postFormHandler pars D.MultiPart = return $ const (return [])

          form :: D.Form Text m a
          form = fromForm Nothing

-- instance (Monad m, FromForm1 m a) => FromRequest m (QueryString a) where
--   requestParser = do
--     form <- requestParser
--     let res = formToQueryString form
--     case res of
--       Nothing -> unexpected "failed"
--       Just t -> return t

-- formToQueryString :: Form a -> Maybe (QueryString a)
-- formToQueryString FormLink = Just $ QueryStringLink
-- fromToQueryString (Form a) = Just $ QueryString a
-- fromToQueryString _        = Nothing

instance (ToURL a, ToURL b) => ToURL (a,b) where
  toURL (a,b)  = toURL a <> toURL b

instance (ToURL a) => ToURL (Maybe a) where
  toURL Nothing  = ""
  toURL (Just t) = toURL t

instance ToURL Text where
  toURL = id

--instance ToURL [Text] where
--  toURL = T.intercalate "/"

instance ToURL String where
  toURL = pack

-- instance ToURL [String] where
  --toURL = toURL . map pack

instance ToURL Int where
  toURL = pack . show

instance ToURL Integer where
  toURL = pack . show

instance (Monad m, FromRequest m a, FromRequest m b) => FromRequest m (a,b) where
  requestParser  = (,) <$> requestParser <*> requestParser

instance (FromRequest m a, Monad m) => FromRequest m (Maybe a) where
  requestParser = optionMaybe (requestParser)

instance Monad m => FromRequest m Text where
  requestParser = anySegment

instance Monad m => FromRequest m [Text] where
  requestParser = many anySegment

instance Monad m => FromRequest m String where
  requestParser = unpack <$> anySegment

instance Monad m => FromRequest m [String] where
  requestParser = many (unpack <$> anySegment)

instance Monad m => FromRequest m Int where
  requestParser = pToken (const "Int") checkIntegral

instance Monad m => FromRequest m Integer where
  requestParser = pToken (const "Integer") checkIntegral

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

  -- fromRequest1 :: Monad m => (Request,[(TL.Text, TL.Text)]) -> m (Maybe a)
  -- fromRequest1 (rq,pars) = do


-- types that can be parsed from a request, maybe
class FromRequest m a where
  requestParser :: URLParser m a

  default requestParser :: (Monad m, Generic a, GFromRequest m (Rep a)) => URLParser m a
  requestParser = to <$> grequestParser

fromRequest :: (Monad m, FromRequest m a) => (Request, FormPars) -> m (Maybe a)
fromRequest (rq, pars) = do
  res <- runParserT requestParser (rq, pars) "" (pathInfo rq)
  return $ case res of
             Left _ -> Nothing
             Right t -> Just t

class ToURL  a where
  toURL :: a -> Text

  default toURL :: (Generic a, GToURL (Rep a)) => a -> Text
  toURL a = "/" <> gtoURL (from a)

instance Monad m => FromRequest m Void where
  requestParser = unexpected "can't produce a void"

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

instance (Monad m, KnownSymbol s, FromRequest m a) => FromRequest m (s :/ a) where
  requestParser = do
    (rq, pars) <- getState
    case pathInfo rq of
      p:ps -> let sv = symbolVal (Proxy::Proxy s) in
              if p ==  pack sv
                then let newrq = rq {pathInfo = ps}
                     in fmap (Key :/) $ putState (newrq, pars) >> requestParser
                else unexpected "failure"
      [] -> unexpected "failure"

instance ToURL () where toURL _ = "/"

instance Monad m => FromRequest m () where
  requestParser = do
    (rq, _) <- getState
    case pathInfo rq of
      [] -> return ()
      [""] -> return ()
      _ -> unexpected "nothing"

--capture any name
data Name a = Name Text a

instance (Monad m, FromRequest m a) => FromRequest m (Name a) where
  requestParser = do
    (rq, pars) <- getState
    case pathInfo rq of
      p:ps -> let newrq = rq {pathInfo = ps}
              in fmap (Name p) $ putState (newrq, pars) >> requestParser
      [] -> unexpected "failure"


instance ToURL a=> ToURL (Name a)
  where toURL (Name nm x) = "/"<> nm <> toURL x

--split on method
data GetOrPost a b = Get a | Post b

instance (Monad m, FromRequest m a, FromRequest m b) => FromRequest m (GetOrPost a b) where
  requestParser = do
    (rq, _) <- getState
    case requestMethod rq of
      "GET" -> fmap Get $ requestParser
      "POST" -> fmap Post $ requestParser

onlyPOST :: Request -> Maybe a -> Maybe a
onlyPOST rq mv = if requestMethod rq == "POST" then mv else Nothing

instance (Monad m, FromRequest m a, FromRequest m b) => FromRequest m (Either a b) where
  requestParser = do
    resL <- Just <$> requestParser <|> return Nothing
    resR <- Just <$> requestParser <|> return Nothing

    case (resL, resR) of
      (Just x, _) -> return $ Left x
      (Nothing, Just y) -> return $ Right y
      _ -> unexpected "failure"

newtype FormFields = FormFields [(TL.Text, TL.Text)]

instance Monad m => FromRequest m Request where
  requestParser = fst <$> getState

instance Monad m => FromRequest m FormFields where
  requestParser = do
    (_, pars) <- getState
    return $ FormFields pars

instance ToURL FormFields where
  toURL _ = ""

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

genericFromForm :: (Generic a, PostFormG m (Rep a), Monad m) => D.Formlet Text m a
genericFromForm def = to <$>  postFormG (from  <$> def)

genericRenderForm :: (Generic a, PostFormG m (Rep a), Monad m) => Proxy a -> Options -> View Text -> HtmlT m ()
genericRenderForm p options view =  do
  DL.errorList "" (toHtml <$> view)
  renderFormG (from <$> p) options view

class FromForm m a where
  fromForm :: D.Formlet Text m a
  default fromForm ::
    (Monad m, Generic a, PostFormG m (Rep a)) => D.Formlet Text m a
  fromForm def = to <$>  postFormG (from  <$> def)

  renderForm :: Proxy a  -> View Text -> HtmlT m ()
  default renderForm :: (Monad m, Generic a, PostFormG m (Rep a)) => Proxy a  -> View Text -> HtmlT m ()
  renderForm p = genericRenderForm p defaultOptions

  getView :: Monad m => Maybe a -> m (View Text)
  getView def = D.getForm "top-level-form" $ fromForm def

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

class FormField m a where
  fromFormField :: D.Formlet Text m a

  renderField :: Monad m => Proxy a -> Text -> Text -> View Text -> HtmlT m ()
  renderField _ = renderBootstrapInput "text" []

---------------------------------------------------------------------------------

class PostFormG m f where
  postFormG :: D.Formlet Text m (f a)
  renderFormG :: Proxy (f a) -> Options -> View Text -> HtmlT m ()

instance (Monad m, PostFormG m f) => PostFormG m (M1 D t f) where
  postFormG def = M1 <$> (postFormG $ unM1 <$> def)
  renderFormG _ = renderFormG (Proxy :: Proxy (f a))

instance (Monad m, PostFormG m f) => PostFormG m (M1 C t f) where
  postFormG def = M1 <$> (postFormG $ unM1 <$> def)
  renderFormG _ = renderFormG (Proxy :: Proxy (f a))

instance (Monad m, Selector t, FormField m a) => PostFormG m (M1 S t (K1 i a)) where
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

instance (Monad m, PostFormG m f, PostFormG m g) => PostFormG m (f :*: g) where
  postFormG (Just (def1 :*: def2)) = (:*:) <$> (postFormG $ Just def1) <*> (postFormG $ Just def2)
  postFormG Nothing = (:*:) <$> (postFormG Nothing) <*> (postFormG Nothing)

  renderFormG _ options view = do
    renderFormG (Proxy :: Proxy (f a)) options view
    renderFormG (Proxy :: Proxy (g a)) options view

instance Monad m => FormField m Text where
  fromFormField = D.text

instance Monad m => FormField m Bool where
  renderField _ fieldName label view = div_ [class_ "checkbox"] $ do
    DL.label fieldName view $ do
      with (DL.inputCheckbox fieldName (toHtml <$> view))
        [autofocus_]
      toHtml label
    DL.errorList fieldName (toHtml <$> view)

  fromFormField = D.bool

instance Monad m => FormField m Int where
  fromFormField = D.stringRead "must be an integer"

instance Monad m => FormField m Double where
  fromFormField = D.stringRead "must be a double"

  renderField _ = renderBootstrapInput "number" []

renderItem :: forall m a. (FromForm a, Monad m)
  => Proxy a -> Text -> View Text -> HtmlT m ()
renderItem _ onclickDelete v = do
  div_ [class_ "youido_multi_item well container"] $ do
    renderForm (Proxy :: Proxy a) v
    fieldButton "Delete" onclickDelete

fieldButton :: Monad m => Text -> Text -> HtmlT m ()
fieldButton name onclick =
  button_ [ type_ "button", onclick_ onclick ] $ do
    a_ [] (toHtml name)

jsCall :: Text -> [Text] -> Text
jsCall fnName args = fnName <> "(" <> T.intercalate "," args <> ")"

jsStr s = "'" <> s <> "'"

instance FromForm a => FormField [a] where
  fromFormField = D.listOf fromForm
  renderField _ fieldName label view = do
    let fieldPath = D.absolutePath fieldName view
        indicesPath = fieldPath ++ [D.indicesRef]
        indicesPathT = D.fromPath indicesPath
        jsArgs = ["this.parentNode", jsStr fieldName]
        onclickDelete = jsCall "youidoRemoveItem" jsArgs <> "; return false;"
        onclickAdd = jsCall "youidoAddItem" jsArgs <> "; return false;"

    DL.label fieldName view $ toHtml label
    div_ [class_ "youido_multi_list form-group"] $ do

      -- Invisible input holds the "indices" path with a value
      input_ [ style_ "display: none"
             , id_ indicesPathT
             , name_ indicesPathT
             , value_ (D.fieldInputText -- NB: relative path required
                       (D.fromPath [fieldName, D.indicesRef]) view)]

      -- Invisible item so that the JS knows how to render
      -- a form when the list is empty
      let
        dummyView = D.makeListSubView fieldName (-1) view
        dummy = renderItem (Proxy :: Proxy a) onclickDelete dummyView
      with dummy [ style_ "display: none"
                 , id_ (D.fromPath fieldPath <> ".youido_dummy_item")]

      traverse_ (renderItem (Proxy :: Proxy a) onclickDelete) $
        D.listSubViews fieldName view
      fieldButton "Add new item" onclickAdd
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
  H :: (FromRequest m a, ToResponse b) => (a -> m b) -> Handler m

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

handle :: (FromRequest m a, ToResponse b, Monad m)
       => (a -> m b) -> YouidoT auth m ()
handle f = handlers %= ((H f):)

unHtmlT :: Monad m => (a -> HtmlT m ()) ->(a -> m AsHtml)
unHtmlT f x = fmap AsHtml $ renderBST $ f x

hHtmlT :: (FromRequest m a, Monad m)
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
  res <- fromRequest rq
  case res of
    Nothing -> run (Youido hs notFound wrapperf lu p) u rq
    Just x -> do
      toResponse . wrapHtml (wrapperf u) <$> f x
