{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric,
             KindSignatures, DataKinds, TypeApplications, GADTs,
             FlexibleInstances, MultiParamTypeClasses, CPP,
             OverloadedLabels, TypeOperators, UndecidableInstances,
             GeneralizedNewtypeDeriving, TemplateHaskell,
             AllowAmbiguousTypes, TypeFamilies #-}

module Youido.Types where

import Debug.Trace
import Youido.Utils
import Network.Wai hiding (Response)
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import Data.Text.Read(signed, decimal)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding as T
import Data.Maybe (maybeToList, isNothing)
import Control.Monad.State.Strict
import Data.Monoid
import GHC.TypeLits
import Data.Proxy
import Lucid
import Data.Aeson hiding (defaultOptions)
import Data.List.Split (split, dropInitBlank, keepDelimsL, whenElt)
import Data.Char (toLower, isUpper)
import Data.List (intercalate, find)
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.OverloadedLabels
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString)
import Network.HTTP.Types
import Data.Void
import Lens.Micro.Platform hiding (to)
import GHC.Generics
import Lucid.PreEscaped
import Control.Applicative((<|>))

import Text.Parsec       (optionMaybe, getState, putState)
import Text.ParserCombinators.Parsec.Pos   (incSourceLine)
import Text.Parsec (ParsecT, runParserT, tokenPrim)
import Text.ParserCombinators.Parsec.Prim  (unexpected, getPosition, (<?>), many)

import Text.Digestive.View (View(..))
import qualified Text.Digestive as D
import qualified Text.Digestive.Form.List as D
import qualified Text.Digestive.Form.Internal as D
import qualified Text.Digestive.Lucid.Html5 as DL

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
  gtoURL   :: f url -> [Text]

instance GToURL U1 where
  gtoURL U1 = [""]

instance GToURL a => GToURL (D1 c a) where
  gtoURL = gtoURL . unM1

instance GToURL a => GToURL (S1 c a) where
  gtoURL = gtoURL . unM1

instance forall c a. (GToURL a, Constructor c) => GToURL (C1 c a) where
  gtoURL m@(M1 x) = (hyphenate . conName) m : gtoURL x

instance (GToURL a, GToURL b) => GToURL (a :*: b) where
  gtoURL (a :*: b) = gtoURL a <> gtoURL b

instance (GToURL a, GToURL b) => GToURL (a :+: b) where
  gtoURL (L1 x) = gtoURL x
  gtoURL (R1 x) = gtoURL x

instance ToURL a => GToURL (K1 i a) where
  gtoURL = toURLSegments . unK1

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
  toURLSegments _ = [""]

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
      runPostForm pars =
        trace (show pars) $
        D.postForm "top-level-form" (formWith pars) (postFormHandler pars)
        where
          postFormHandler :: (Monad m) => [(TL.Text, TL.Text)] -> D.FormEncType -> m (D.Env m)
          postFormHandler pars D.UrlEncoded = do
            traceM $ TL.unpack (TL.intercalate "\n" (map (\(t,s) -> "(" <> t <> ", " <> s <> ")") pars))
            return $ \path -> (return $ lookupPath pars path)
          postFormHandler pars D.MultiPart = return $ const (return [])

          formWith :: [(TL.Text, TL.Text)] -> D.Form Text m a
          formWith pars =
            let sums = M.fromList $
                  map (\(k,v) -> (TL.toStrict . snd $ TL.breakOnEnd "youido-sums." k,
                                  TL.toStrict v))
                  . filter (\(k,v) -> "youido-sums." `TL.isPrefixOf` k) $ pars
            in trace ("parsed sums: " <> show sums) $ fromForm (Just sums) Nothing

getList :: TL.Text -> [(TL.Text, TL.Text)] -> [TL.Text]
getList k = map snd . filter (\x -> fst x == k)

instance (ToURL a, ToURL b) => ToURL (a,b) where
  toURLSegments (a,b)  = toURLSegments a <> toURLSegments b

instance (ToURL a) => ToURL (Maybe a) where
  toURLSegments Nothing  = []
  toURLSegments (Just t) = toURLSegments t

instance ToURL Text where
  toURLSegments = (:[])

--instance ToURL [Text] where
--  toURLSegments = T.intercalate "/"

instance ToURL String where
  toURLSegments = (:[]) . pack

-- instance ToURL [String] where
  --toURLSegments = toURLSegments . map pack

instance ToURL Int where
  toURLSegments = (:[]) . pack . show

instance ToURL Integer where
  toURLSegments = (:[]) . pack . show

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
  toURLSegments :: a -> [Text]

  default toURLSegments :: (Generic a, GToURL (Rep a)) => a -> [Text]
  toURLSegments a = gtoURL (from a)

toURL :: (ToURL a) => a -> Text
toURL x = T.intercalate "/" ("" : toURLSegments x)

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
  toURLSegments (_ :/ a) = (pack $ symbolVal (Proxy::Proxy s)) : toURLSegments a

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

instance ToURL () where toURLSegments _ = [""]

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
  where toURLSegments (Name nm x) = nm : toURLSegments x

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
  toURLSegments _ = []

--------------------------------------------------------------------------
---                 FORM HANDLING
--------------------------------------------------------------------------

data Options a = Options
  {
    fieldLabelModifier :: Text -> Text
  , constructorTagModifier :: Text -> Text
  , sumConstructor :: Maybe Text
  , ctorMap :: Map Text Text
  , viewOf :: Maybe a
  }

instance Functor Options where
  fmap f o = o { viewOf = f <$> viewOf o }

-- for the many generic instance functions which don't need the viewOf element
nextOpts :: Options a -> Options b
nextOpts o = o { viewOf = Nothing }

defaultOptions :: Options a
defaultOptions = Options id id Nothing M.empty Nothing

genericFromForm :: (Generic a, PostFormG m (Rep a), Monad m)
                => Maybe SumChoices -> D.Formlet Text m a
genericFromForm ch def = to <$>  postFormG ch (from  <$> def)

genericRenderForm :: (Generic a, PostFormG m (Rep a), Monad m)
                  => Proxy a -> Options a -> View Text -> HtmlT m ()
genericRenderForm p options view =  do
  DL.errorList "" (toHtml <$> view)
  renderFormG (from <$> p) (from <$> options) view

renderSumForm :: forall a m. (Monad m, Generic a, PostFormG m (Rep a))
              => Maybe (Options a) -> Maybe a -> View Text -> HtmlT m ()
renderSumForm mopts mdef v = do
  let opts = (maybe defaultOptions id mopts) { viewOf = mdef}
  genericRenderForm (Proxy :: Proxy a) opts v

class FromForm m a where
  fromForm :: Maybe SumChoices -> D.Formlet Text m a
  default fromForm ::
    (Monad m, Generic a, PostFormG m (Rep a)) => Maybe SumChoices -> D.Formlet Text m a
  fromForm ch def = to <$>  postFormG ch (from  <$> def)

  renderForm :: Proxy a -> View Text -> HtmlT m ()
  default renderForm :: (Monad m, Generic a, PostFormG m (Rep a))
                     => Proxy a -> View Text -> HtmlT m ()

  renderForm p = genericRenderForm p defaultOptions

  getView :: Monad m => Maybe a -> m (View Text)
  getView def = D.getForm "top-level-form" $ fromForm Nothing def

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

---------------------------------------------------------------------------------

class PostFormG m f where
  postFormG :: Maybe SumChoices -> D.Formlet Text m (f a)
  renderFormG :: Proxy (f a) -> Options (f a) -> View Text -> HtmlT m ()

instance (Monad m, PostFormG m f) => PostFormG m (M1 D t f) where
  postFormG ch def = M1 <$> (postFormG ch $ unM1 <$> def)
  renderFormG _ opts = renderFormG (Proxy :: Proxy (f a)) (unM1 <$> opts)

instance (Monad m, Constructor c, PostFormG m f) => PostFormG m (M1 C c f) where
  postFormG ch def = M1 <$> (postFormG ch $ unM1 <$> def)
  renderFormG _ opts = renderFormG (Proxy :: Proxy (f a)) (unM1 <$> opts)

instance {-# OVERLAPS #-} (Monad m, Constructor c) => PostFormG m (M1 C c U1) where
  postFormG ch def = M1 <$> cname  D..: (const U1 <$> D.optionalText Nothing)
    where cname = pack $ conName (undefined :: M1 C c U1 ())
  renderFormG _ opts v = do
    traceM . unpack $ "renderFieldG #D1, viewpaths: " <> T.intercalate ", " (map D.fromPath $ D.debugViewPaths v)
    div_ [style_ "display: inherit"] $ "C1 fld: " <> toHtml cname <> " (pfg M1 C U1)"
    where cname = pack $ conName (undefined :: M1 C c U1 ())

instance (Monad m, Selector t, FormField m a) => PostFormG m (M1 S t (K1 i a)) where
  postFormG ch def = do
    trace (show $ "postFormG#S1-K1 for field " <> show fieldName <> ", cons " <> show mctor) $
      M1 .K1 <$> fieldName D..: (fromFormField mctor $ unK1 . unM1  <$> def)
   where
     val :: M1 S t (K1 i a) r
     val = undefined
     fieldName :: Text
     fieldName = T.pack $ selName val
     mctor = M.lookup fieldName =<< ch

  renderFormG _ options view = do
    traceM $ show view
    traceM $ show $ preV view
    renderField (Proxy :: Proxy a) (unK1 . unM1 <$> options)
      fieldName (fieldLabelModifier options $ fieldName) view
   where
     val :: M1 S t (K1 i a) r
     val = undefined

     fieldName :: Text
     fieldName = T.pack $ selName val

debugPaths :: View Text -> Text
debugPaths = T.intercalate ", " . map D.fromPath . D.debugViewPaths

instance (Monad m, PostFormG m f, PostFormG m g) => PostFormG m (f :*: g) where
  postFormG ch (Just (def1 :*: def2)) = (:*:) <$> (postFormG ch $ Just def1) <*> (postFormG ch $ Just def2)
  postFormG ch Nothing = (:*:) <$> (postFormG ch Nothing) <*> (postFormG ch Nothing)

  renderFormG _ options view = do
    traceM . unpack $ "renderFormG @(f :+: g), viewpaths: " <> debugPaths view
    renderFormG (Proxy :: Proxy (f a)) optsf view
    renderFormG (Proxy :: Proxy (g a)) optsg view
      where (optsf, optsg) = case viewOf options of
              Just (ff :*: gg) -> (options {viewOf = Just ff}, options {viewOf = Just gg})
              _ -> (options {viewOf = Nothing}, options {viewOf = Nothing})

instance (Monad m, HasConName f, HasConName g, PostFormG m f, PostFormG m g) => PostFormG m (f :+: g) where
  postFormG ch (Just (L1 def)) = L1 <$> (postFormG ch (Just def))
  postFormG ch (Just (R1 def)) = R1 <$> (postFormG ch (Just def))
  postFormG ch Nothing = L1 <$> postFormG ch Nothing --TODO: fix needed?
  -- postFormG Nothing = R1 <$> postFormG Nothing
  renderFormG _ opts v@(View _ _ form _ _ _) = do
    traceM $ "renderFormG #f:+:g, rendering for ctor " <> (show $ sumConstructor opts)
    let mctor = (\c -> (c, hasConName @f c)) <$> sumConstructor opts
    case mctor of
      Just (c, CtorFoundDirect _) -> -- take subview only on this case?
        renderFormG (Proxy :: Proxy (f ())) (nextOpts opts) v -- (D.subView c v)
      Just (c, CtorFoundNested _) ->
        renderFormG (Proxy :: Proxy (f ())) (nextOpts opts) v --(D.subView ctor v)
      Just (c, CtorNotFound) ->renderFormG (Proxy :: Proxy (g ())) (nextOpts opts) v
       -- case hasConName @g c of
          -- (CtorFoundDirect _) -> renderFormG (Proxy :: Proxy (g ())) opts (D.subView c v)
          -- _ -> renderFormG (Proxy :: Proxy (g ())) opts v
      Nothing -> error $ "renderFormG on :+:, ctor not found: " <> (unpack . preV $ v) <> " ** " <> show v

preV :: View Text -> Text
preV (View nm ctx frm inp errs med) = mconcat
  [ "viewName: ", nm, ", viewContext: ", D.fromPath ctx]

--------------------

class FormField m a where
  fromFormField :: Maybe Text -> D.Formlet Text m a
  renderField :: (Monad m) => Proxy a -> Options a -> Text -> Text -> View Text -> HtmlT m ()

  default fromFormField :: (Monad m, Generic a, FormFieldG m (Rep a)) => Maybe Text -> D.Formlet Text m a
  fromFormField = fromFormFieldG'

  default renderField :: (Monad m, Generic a, FormFieldG m (Rep a)) => Proxy a -> Options a -> Text -> Text -> View Text -> HtmlT m ()
  renderField = renderFieldG'

fromFormFieldG' :: forall a m. (Monad m, Generic a, FormFieldG m (Rep a))
                => Maybe Text -> D.Formlet Text m a
fromFormFieldG' mctor mdef = to <$> fromFormFieldG mctor (from <$> mdef)

renderFieldG' :: forall a s m. (Monad m, Generic a, FormFieldG m (Rep a))
              => Proxy a -> Options a -> Text -> Text -> View Text -> HtmlT m ()
renderFieldG' _ opts = renderFieldG (Proxy :: Proxy (Rep a ())) (from <$> opts)

class FromFormSumG m f where
  fromFormSumG :: Text -> D.Formlet Text m (f p)

instance (Monad m, FromFormSumG m f) => FromFormSumG m (D1 d f) where
  fromFormSumG ctor mdef = M1 <$> fromFormSumG ctor (unM1 <$> mdef)

instance (Monad m, HasConName f, FromFormSumG m f, FromFormSumG m g) => FromFormSumG m (f :+: g) where
  fromFormSumG :: Text -> D.Formlet Text m ((f :+: g) p)
  fromFormSumG ctor (Just (L1 def)) = L1 <$> fromFormSumG ctor (Just def)
  fromFormSumG ctor (Just (R1 def)) = R1 <$> fromFormSumG ctor (Just def)
  fromFormSumG ctor Nothing =
    case hasConName @f ctor of
      CtorFoundDirect _ ->
        L1 <$> fromFormSumG @m @f ctor Nothing
      CtorFoundNested _ ->
        L1 <$> fromFormSumG @m @f ctor Nothing
      CtorNotFound ->
        R1 <$> fromFormSumG @m @g ctor Nothing
    -- error $ "fromSumFormG on :+:, " <> (unpack . preV $ v) <> " ** " <> show v

instance {-# OVERLAPPING #-} (Monad m, Constructor c) => FromFormSumG m (C1 c U1) where
  fromFormSumG ctor def = M1 <$> (const U1 <$> D.optionalText Nothing)
    where cname = pack $ conName (undefined :: C1 c U1 ())

instance {-# OVERLAPPABLE #-} (Monad m, PostFormG m f, Constructor c) => FromFormSumG m (C1 c f) where
  fromFormSumG ctor def = M1 <$> postFormG (Just $ M.fromList [("",ctor)]) (unM1 <$> def)
    where cname = pack $ conName (undefined :: C1 c U1 ())

-- Map from form paths to (datatype name, constructor name)
-- Instructs formtree builder (fromForm* funcs) which path in the sum to build
type SumChoices = Map Text Text

class FormFieldG m f where
  fromFormFieldG :: Maybe Text -> Maybe (f p) -> D.Form Text m (f p)
  renderFieldG :: (Monad m) => Proxy (f p) -> Options (f p) -> Text -> Text -> View Text -> HtmlT m ()

-- This instance is only intended for sum types
instance (Monad m, FromFormSumG m f, PostFormG m f, GetConNameG f, EnumCtors f, Datatype d) => FormFieldG m (D1 d f) where
  fromFormFieldG mctor mdef = trace (show mctor) $
    case mdef of -- default value takes precedence over ctor
      Just x -> fromFormSumG (getConNameG x) mdef
      Nothing -> case mctor of
        Just ctor -> fromFormSumG ctor Nothing
          -- No default and no choice made => indicate this with an unused dummy form
        _ -> D.disable $ "disabled" D..: fromFormSumG (head $ enumCtors @f) Nothing
    where nm = pack $ datatypeName (undefined :: D1 d f ())

  renderFieldG _ opts fldNm label v@(View n ctx frm inps es meth) = do
    let dtNm = pack $ datatypeName (undefined :: D1 d f ())
        ctors = enumCtors @f
        sumcon = getConNameG <$> viewOf opts
        onclick = jsCall "youido-select-constructor" []
    views <- lift $ mkSumViews @(D1 d f) n dtNm ctors
    traceM $ "renderFieldG #D1, datatype: " <> show dtNm <> ", ctors: " <> show ctors
    traceM . unpack $ "renderFieldG #D1, view: " <> preV v
    traceM $ "renderFieldG #D1, viewpaths: " <> show (map D.fromPath $ D.debugViewPaths v)
    div_ [class_ "container"] $ do
      DL.label dtNm v $ toHtml fldNm
      -- render select dropdown for each constructor
      -- TODO: add disabled 'Select one..' default
      select_ [onchange_ "youido-select-constructor(this)"] $
        traverse_ (option_ [] . toHtml) ctors
      -- render all possible sum paths invisibly, but only 0 or 1 will be shown
      flip traverse_ (zip ctors views) $ \(ctor, sumv) ->
        let dummy = renderFormG (Proxy :: Proxy (f ()))
                    (opts {sumConstructor = Just ctor, viewOf = Nothing}) sumv
        in with dummy [style_ "display: none", data_ "constructor" ctor]

      -- Render the 'real' form if one has been selected
      case sumcon of
        Just con -> do
          traceM $ "rendering real form for sum" <> show sumcon
          traverse_ (traceM . show) $ map show (D.subViews v)
          when (not $ D.viewDisabled "disabled" v) $
            renderFormG (Proxy :: Proxy (f ())) (opts {sumConstructor = Just "Blue",
                                                      viewOf = Nothing}) (D.subView fldNm v)
        Nothing -> error "no ctor found to render"

mkSumViews :: forall f m. (FromFormSumG m f, FormFieldG m f, Monad m)
           => Text -> Text -> [Text] -> m [View Text]
mkSumViews viewNm adtNm ctors =
  traverse (\c -> D.getForm viewNm $ fromFormFieldG @m @f (Just c) Nothing) ctors

instance Monad m => FormField m Text where
  fromFormField _ = D.text
  renderField _ _ = renderBootstrapInput "text" []

instance Monad m => FormField m Bool where
  renderField _ _ fieldName label view = div_ [class_ "checkbox"] $ do
    DL.label fieldName view $ do
      with (DL.inputCheckbox fieldName (toHtml <$> view))
        [autofocus_]
      toHtml label
    DL.errorList fieldName (toHtml <$> view)

  fromFormField _ = D.bool

instance Monad m => FormField m Int where
  fromFormField _ = D.stringRead "must be an integer"
  renderField _ _ = renderBootstrapInput "text" []

instance Monad m => FormField m Double where
  fromFormField _ = D.stringRead "must be a double"

  renderField _ _ = renderBootstrapInput "number" []

renderItem :: forall m a. (FromForm m a, Monad m)
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

instance (FromForm m a, Monad m) => FormField m [a] where
  fromFormField _ = D.listOf $ fromForm Nothing
  renderField _ _ fieldName label view = do
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
