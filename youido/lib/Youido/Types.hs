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
import qualified Data.Text.Encoding as T
import Data.Maybe (maybeToList)
import Control.Monad.State.Strict
import Data.Monoid
import GHC.TypeLits
import Data.Proxy
import Lucid
import Data.Aeson hiding (defaultOptions)
import Data.List.Split (split, dropInitBlank, keepDelimsL, whenElt)
import Data.Char (toLower, isUpper)
import Data.List (intercalate, partition)
import Data.Foldable (traverse_)
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
import qualified Text.Digestive.Form.Internal as D
import qualified Text.Digestive.Types as D (FormInput(..))
import qualified Text.Digestive.Form.List as D
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

-- Map from form paths to (datatype name, constructor name)
-- Instructs formtree builder (fromForm* funcs) which path in the sum to build
type SumChoices = Map Text Text
notSelectedValue = "#not-selected" :: Text
renderCtorKey = "#render-now" :: Text

mkSumChoices :: [(TL.Text, TL.Text)] -> SumChoices
mkSumChoices = M.fromList
  . map (\(k,v) -> (TL.toStrict . snd $ TL.breakOnEnd "youido-sums." k,
                    TL.toStrict v))
  . filter (\(k,v) -> "youido-sums." `TL.isPrefixOf` k
                      && not (".-1." `TL.isInfixOf` k)) -- no dummy list

-- Manually inserts errors for sum types where no selection was made
withSelectionErrors :: View Text -> SumChoices -> View Text
withSelectionErrors v@(View _ _ _ _ es _) choices =
  v { viewErrors = selErrs ++ es }
  where selErrs =
          map (\(k,v) -> (D.toPath k, "Selection required"))
          . M.toList $ M.filter ((==) notSelectedValue) choices

annotateWithSumChoices :: View Text -> SumChoices -> View Text
annotateWithSumChoices v@(View _ _ _ input es _) choices =
  v { viewInput = selInput ++ input
    , viewErrors = selErrs ++ es }
  where selInput = map (\(k,v) -> (D.toPath k ++ ["#choice"], D.TextInput v)) selected
        selErrs = map (\(k,v) -> (D.toPath k, "Selection required")) notSelected
        (selected, notSelected) = partition (\(k,v) -> v /= notSelectedValue) $ M.toList choices

instance (Monad m, FromForm m a) => FromRequest m (Form a) where
  requestParser = do
    result <- formResult
    traceM $ "**** result errors: " <> show (viewErrors $ fst result)
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
      runPostForm pars = do
        (v, answer) <- D.postForm "top-level-form" form (postFormHandler pars)
        let v' = annotateWithSumChoices v choices
        return (v', if null (viewErrors v') then answer else Nothing)
        where
          postFormHandler :: (Monad m) => [(TL.Text, TL.Text)] -> D.FormEncType -> m (D.Env m)
          postFormHandler pars D.UrlEncoded = return $ \path -> (return $ lookupPath pars path)
          postFormHandler pars D.MultiPart = return $ const (return [])
          choices = mkSumChoices pars
          opts = FromFormOptions $
                 Just (SumOptions (M.filter ((/=) notSelectedValue) choices) Nothing "")
          form :: D.Form Text m a
          form = fromForm' opts Nothing

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

-- TODO: make sumCtor and viewOf an Either, or can they both be present?
-- TODO: rename to something like RenderOptions
data Options a = Options
  {
    fieldLabelModifier :: Text -> Text
  , constructorTagModifier :: Text -> Text
  , sumConstructor :: Maybe Text -- | Rennders specifc ctor
  , ctorMap :: SumChoices -- | (maybe.context.)fieldName -> Constructor
  , viewOf :: Maybe a -- | Renders `a` when present
  }

instance Functor Options where
  fmap f o = o { viewOf = f <$> viewOf o }

nextOpts :: Options a -> Options b
nextOpts o = o { viewOf = Nothing }

defaultOptions :: Options a
defaultOptions = Options id id Nothing M.empty Nothing

data SumOptions = SumOptions
  { sumChoices :: SumChoices
  , currentlyBuildingCtor :: Maybe Text
  , currentContext :: Text
  } deriving Show

data FromFormOptions = FromFormOptions
  { sumOpts :: Maybe SumOptions
  } deriving Show

mkSumOpts ch ctor = FromFormOptions $ Just (SumOptions ch (Just ctor) "") -- TODO: fix
mkSumOptsCtx ch ctor ctx = FromFormOptions $ Just (SumOptions ch (Just ctor) ctx)

defaultFromFormOpts = FromFormOptions Nothing

genericFromForm :: (Generic a, PostFormG m (Rep a), Monad m)
  => FromFormOptions -> D.Formlet Text m a
genericFromForm opts def = to <$>  postFormG opts (from  <$> def)

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
  fromForm' :: FromFormOptions -> D.Formlet Text m a
  default fromForm' ::
    (Monad m, Generic a, PostFormG m (Rep a)) => FromFormOptions -> D.Formlet Text m a
  fromForm' = genericFromForm

  fromForm :: D.Formlet Text m a
  fromForm = fromForm' defaultFromFormOpts

  renderForm' :: Proxy a -> Options a -> View Text -> HtmlT m ()
  default renderForm' :: (Monad m, Generic a, PostFormG m (Rep a))
                     => Proxy a -> Options a -> View Text -> HtmlT m ()
  renderForm' = genericRenderForm

  renderForm :: Proxy a -> View Text -> HtmlT m ()
  renderForm p = renderForm' p defaultOptions

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

---------------------------------------------------------------------------------

class PostFormG m f where
  postFormG :: FromFormOptions -> D.Formlet Text m (f a)
  renderFormG :: Proxy (f a) -> Options (f a) -> View Text -> HtmlT m ()

instance (Monad m, PostFormG m f) => PostFormG m (M1 D t f) where
  postFormG opts def = M1 <$> (postFormG opts $ unM1 <$> def)
  renderFormG _ opts = renderFormG (Proxy :: Proxy (f a)) (unM1 <$> opts)

instance (Monad m, Constructor c, PostFormG m f) => PostFormG m (M1 C c f) where
  postFormG opts def = M1 <$> (postFormG opts $ unM1 <$> def)
  renderFormG _ opts = renderFormG (Proxy :: Proxy (f a)) (unM1 <$> opts)

instance {-# OVERLAPS #-} (Monad m, Constructor c) => PostFormG m (M1 C c U1) where
  postFormG _ _ = pure $ M1 U1
  renderFormG _ opts v = span_ [class_ $ "youido-u1" <> " youido-u1-" <> cname] ""
    where cname = pack $ conName (undefined :: M1 C c U1 p)

instance (Monad m, Selector t, FormField m a) => PostFormG m (M1 S t (K1 i a)) where
  postFormG opts def =
    trace (show $ "***** postFormG#S1-K1 for field " <> show fieldName
           <> ", cons " <> show mctor
           <> ", \n >>>lookup map: " <> show mchoices
          ) $
      M1 . K1 <$> subformNm D..: (fromFormField opts' $ unK1 . unM1  <$> def)
   where
     fieldName = T.pack $ selName (undefined :: (M1 S t (K1 i a) r))
     mchoices = sumChoices <$> sumOpts opts
     ctx = maybe "" (\c -> let cc = currentContext c in
                        if "." `T.isSuffixOf` cc || cc == "" then cc else cc <> ".") $ sumOpts opts
     k = ctx <> fieldName
     mctor = trace ("<<<>>>> k: " <> unpack k) (M.lookup k) =<< mchoices -- only place choices is used
     subformNm = if fieldName /= "" then fieldName else "none"
     newCtx = ctx <> fieldName
     opts' = opts {sumOpts = Just $ SumOptions (maybe M.empty id mchoices) mctor newCtx}

  renderFormG _ options view =
    renderField (Proxy :: Proxy a) (unK1 . unM1 <$> options)
      fieldName (fieldLabelModifier options $ fieldName) view
   where
     fieldName = T.pack $ selName (undefined :: M1 S t (K1 i a) r)

debugPaths :: View Text -> Text
debugPaths = T.intercalate ", " . map D.fromPath . D.debugViewPaths

instance (Monad m, PostFormG m f, PostFormG m g) => PostFormG m (f :*: g) where
  postFormG opts (Just (def1 :*: def2)) =
    (:*:) <$> (postFormG opts $ Just def1) <*> (postFormG opts $ Just def2)
  postFormG opts Nothing = (:*:) <$> (postFormG opts Nothing) <*> (postFormG opts Nothing)

  renderFormG _ options view = do
    traceM . unpack $ "renderFormG @(f :+: g), viewpaths: " <> debugPaths view
    renderFormG (Proxy :: Proxy (f a)) optsf view
    renderFormG (Proxy :: Proxy (g a)) optsg view
      where (optsf, optsg) = case viewOf options of
              Just (ff :*: gg) -> (options {viewOf = Just ff}, options {viewOf = Just gg})
              _ -> (options {viewOf = Nothing}, options {viewOf = Nothing})

instance (Monad m, HasConName f, HasConName g, PostFormG m f, PostFormG m g) => PostFormG m (f :+: g) where
  postFormG opts (Just (L1 def)) = L1 <$> postFormG opts (Just def)
  postFormG opts (Just (R1 def)) = R1 <$> postFormG opts (Just def)
  postFormG opts@(FromFormOptions (Just (SumOptions _ (Just ctor) _))) Nothing =
    if hasConName @f ctor
    then L1 <$> postFormG @m @f opts Nothing
    else if hasConName @g ctor
         then R1 <$> postFormG @m @g opts Nothing
         else error $ "fromSumFormG, ctor not found: " <> unpack ctor
  postFormG opts@(FromFormOptions (Just (SumOptions _ Nothing _))) Nothing =
    error "possible youido bug: currentlyBuildingCtor opt must be present to build sum type"

  renderFormG _ opts v = do
    case (\c -> (hasConName @f c, hasConName @g c)) <$> sumConstructor opts of
      Just (True, _) -> renderFormG (Proxy :: Proxy (f ())) (nextOpts opts) v
      Just (_, True) -> renderFormG (Proxy :: Proxy (g ())) (nextOpts opts) v
      Just (False, False) -> DL.errorList "Selection is required" (toHtml <$> v)
      Nothing -> error "Youido bug: absent sumConstructor opt is required to render sum type"

preV :: View Text -> Text
preV (View nm ctx frm inp errs med) = mconcat
  [ "viewName: ", nm, ", viewContext: ", D.fromPath ctx]

--------------------

class FormField m a where
  fromFormField :: FromFormOptions -> D.Formlet Text m a
  renderField :: (Monad m) => Proxy a -> Options a -> Text -> Text -> View Text -> HtmlT m ()

  default fromFormField :: (Monad m, Generic a, FormFieldG m (Rep a))
                        => FromFormOptions -> D.Formlet Text m a
  fromFormField = fromFormFieldG'

  default renderField :: (Monad m, Generic a, FormFieldG m (Rep a))
                      => Proxy a -> Options a -> Text -> Text -> View Text -> HtmlT m ()
  renderField = renderFieldG'

fromFormFieldG' :: forall a m. (Monad m, Generic a, FormFieldG m (Rep a))
  => FromFormOptions -> D.Formlet Text m a
fromFormFieldG' mctor mdef = to <$> fromFormFieldG mctor (from <$> mdef)

renderFieldG' :: forall a s m. (Monad m, Generic a, FormFieldG m (Rep a))
  => Proxy a -> Options a -> Text -> Text -> View Text -> HtmlT m ()
renderFieldG' _ opts = renderFieldG (Proxy :: Proxy (Rep a ())) (from <$> opts)

class FromFormSumG m f where
  fromFormSumG :: FromFormOptions -> D.Formlet Text m (f p)

instance (Monad m, FromFormSumG m f) => FromFormSumG m (D1 d f) where
  fromFormSumG ctor mdef = M1 <$> fromFormSumG ctor (unM1 <$> mdef)

instance (Monad m, HasConName f, HasConName g, FromFormSumG m f, FromFormSumG m g) => FromFormSumG m (f :+: g) where
  fromFormSumG :: FromFormOptions -> D.Formlet Text m ((f :+: g) p)
  fromFormSumG ctor (Just (L1 def)) = L1 <$> fromFormSumG ctor (Just def)
  fromFormSumG ctor (Just (R1 def)) = R1 <$> fromFormSumG ctor (Just def)
  fromFormSumG opts@(FromFormOptions (Just (SumOptions _ (Just ctor) _))) Nothing =
    if hasConName @f ctor
    then L1 <$> fromFormSumG @m @f opts Nothing
    else if hasConName @g ctor
         then R1 <$> fromFormSumG @m @g opts Nothing
         else error $ "fromSumFormG, ctor not found: " <> unpack ctor

instance {-# OVERLAPPING #-} (Monad m) => FromFormSumG m (C1 c U1) where
  fromFormSumG _ def = pure $ M1 U1

instance {-# OVERLAPPABLE #-} (Monad m, PostFormG m f) => FromFormSumG m (C1 c f) where
  fromFormSumG opts def = M1 <$> postFormG opts (unM1 <$> def)

class FormFieldG m f where
  fromFormFieldG :: FromFormOptions -> Maybe (f p) -> D.Form Text m (f p)
  renderFieldG :: (Monad m) => Proxy (f p) -> Options (f p) -> Text -> Text -> View Text -> HtmlT m ()

mkSumViews :: forall f m. (FormFieldG m f, Monad m)
           => SumChoices -> D.Path -> [Text] -> m [View Text]
mkSumViews chs subviewPath ctors =
  let newCtx = D.fromPath . tail . tail $ subviewPath in -- drops "dummy" and view name
  traverse (\c -> D.getForm (D.fromPath subviewPath)
                  $ fromFormFieldG @m @f (mkSumOptsCtx chs c newCtx) Nothing) ctors

findChoice :: D.Path -> [(D.Path, D.FormInput)] -> Maybe Text
findChoice path input =
  case filter (\(k,_) -> k == choicePath) input of
    [(_,D.TextInput ctor)] -> Just ctor
    _ -> Nothing
  where choicePath = path ++ ["#choice"]

instance (Monad m, PostFormG m f,
          GetConNameG f, HasConName f, EnumCtors f, Datatype d, f ~ (g :+: h))
         => FormFieldG m (D1 d f) where
  fromFormFieldG opts@(FromFormOptions (Just (SumOptions ch mctor ctx))) mdef =
    case mdef of -- default value takes precedence over ctor
      Just x -> postFormG (mkSumOptsCtx ch (getConNameG x) ctx) mdef
      Nothing -> case mctor of
        Just ctor -> postFormG (mkSumOptsCtx ch ctor ctx) Nothing
          -- No default and no choice made => a dummy form is required
        _ -> "disabled" D..: postFormG (mkSumOptsCtx ch (head $ enumCtors @f) ctx) Nothing
    where nm = pack $ datatypeName (undefined :: D1 d f ())
  fromFormFieldG (FromFormOptions Nothing) _ =
    error "possible youido bug: options required for rendering a sum type form"

  renderFieldG _ opts fldNm label v@(View viewNm ctx _ input es _) = do
    let dtNm = pack $ datatypeName (undefined :: D1 d f ())
        ctors = enumCtors @f
        onselect = jsCall "youidoSelectConstructor"
                   ["this", jsStr viewNm, jsStr (D.fromPath ctx), jsStr fldNm]
        defaultOpt =  "Select one..." :: Text
        selectedOpt = maybe defaultOpt id sumCtor
        selAttr = \ctor -> if ctor == selectedOpt then [selected_ "true"] else []
        fieldPath = viewNm : ctx ++ [fldNm]
        sumCtor = (getConNameG <$> viewOf opts) <|> findChoice (tail fieldPath) input
        fieldRef = D.fromPath fieldPath
        ctorChoiceInputId = D.fromPath $ "youido-sums" : tail fieldPath
        subviewPath = "dummy" : fieldPath

    views <- lift $ mkSumViews @(D1 d f) (ctorMap opts) subviewPath ctors
    traceM $ "renderFieldG #D1, datatype: " <> show dtNm <> ", ctors: " <> show ctors <> ", ctor: " <> show sumCtor
    traceM . unpack $ "renderFieldG #D1, view: " <> preV v
    traceM $ "renderFieldG #D1, viewpaths: " <> show (map D.fromPath $ D.debugViewPaths v)

    div_ [class_ "form-group"] $ do
      DL.label fldNm v $ toHtml label
      select_ [onchange_ onselect, class_ "form-control"] $ do
        option_ (selAttr defaultOpt ++ [disabled_ "true", value_ defaultOpt]) $ toHtml defaultOpt
        flip traverse_ ctors $ \c -> option_ (selAttr c) $ toHtml c
      DL.errorList fldNm (toHtml <$> v)

      input_ [ style_ "display: none"
             , id_ ctorChoiceInputId
             , name_ ctorChoiceInputId
             , value_ $ maybe notSelectedValue id sumCtor]

      flip traverse_ (zip ctors views) $ \(ctor, sumv) ->
        div_ [ style_ "display: none"
             , class_ $ if hasU1ConName @f ctor then "youido-u1-container" else ""
             , id_ ("youido-sum-dummy-" <> fieldRef <> "." <> ctor)
             , data_ "constructor" ctor] $
          renderFormG (Proxy :: Proxy (f ()))
            (opts {sumConstructor = Just ctor, viewOf = Nothing}) sumv

      div_ [ class_ "well container"
           , id_ $ "youido-sum-real-container-" <> fieldRef
           , style_ $ if null sumCtor || maybe False (hasU1ConName @f) sumCtor
                      then "display:none" else ""] $
        flip traverse_ sumCtor $ \con -> do
          traceM $ "rendering real form for sum" <> show sumCtor
          traverse_ (traceM . show) $ map show (D.subViews v)
          renderFormG (Proxy :: Proxy (f ()))
            (opts {sumConstructor = Just con, viewOf = Nothing})
            $ D.subView fldNm v

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
  => Proxy a -> Options a -> Text -> View Text -> HtmlT m ()
renderItem p opts onclickDelete v = do
  div_ [class_ "youido_multi_item well container"] $ do
    renderForm' p opts v
    fieldButton "Delete" onclickDelete

fieldButton :: Monad m => Text -> Text -> HtmlT m ()
fieldButton name onclick =
  button_ [ type_ "button", onclick_ onclick ] $ do
    a_ [] (toHtml name)

jsCall :: Text -> [Text] -> Text
jsCall fnName args = fnName <> "(" <> T.intercalate "," args <> ")"

jsStr s = "'" <> s <> "'"

-- copied from Text.Digestive.Form because it's not exported
listIndices :: (Monad m, Monoid v) => [Int] -> D.Form v m [Int]
listIndices = fmap D.parseIndices . D.text . Just . D.unparseIndices

instance (FromForm m a, Monad m) => FormField m [a] where
  fromFormField opts def =
    -- opts should contain the field name of this list
    -- then on each list item we should add the index to 'current context'
    D.List defList (D.indicesRef D..: listIndices ixs) -- fromForm' opts
    where ixs = case def of
            Nothing -> trace "!!!!!!!! list def is Nothing" [0]
            Just xs -> trace ("!!!!!!! list def is length " <> show (length xs))[0 .. length xs - 1]
          defItems = maybe [] (map Just) def
          defList = D.DefaultList
                     (fromForm' (withCtx (-1)) Nothing)
                     (map (\(defItem,i) -> fromForm' (withCtx i) defItem) $ zip defItems ixs)
          ctx = maybe "" currentContext $ sumOpts opts
          curCtx = if ctx /= "" then ctx <> "." else ""
          withCtx n = (opts { sumOpts =
                             (\o -> o{ currentContext = curCtx <> pack (show n)})
                             <$> (sumOpts opts)
                           })
  renderField _ opts fieldName label view = do
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
        dummy = renderItem (Proxy :: Proxy a) (nextOpts opts) onclickDelete dummyView
      with dummy [ style_ "display: none"
                 , id_ (D.fromPath fieldPath <> ".youido_dummy_item")]

      let subviews = D.listSubViews fieldName view
          viewsOf = map Just <$> (viewOf opts)
      flip traverse_ (zip subviews (maybe (repeat Nothing) id viewsOf)) $ \(subv, viewof) ->
        renderItem (Proxy :: Proxy a) (opts {viewOf = viewof}) onclickDelete subv

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
