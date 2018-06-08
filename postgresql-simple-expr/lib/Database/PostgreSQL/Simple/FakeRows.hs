{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric,
             DefaultSignatures, PolyKinds, TypeOperators, ScopedTypeVariables,
             FlexibleContexts, FlexibleInstances, OverloadedStrings, InstanceSigs,
             TypeApplications, AllowAmbiguousTypes, UndecidableInstances #-}
module Database.PostgreSQL.Simple.FakeRows
  ( FakeRows(..)
  , generateRows
  , genericPopulateNoKey
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Dynamic
import           Data.Foldable
import           Data.List (intersperse)
import           Data.Map (Map(..))
import qualified Data.Map as M
import           Data.Monoid ((<>), mconcat)
import           Data.Proxy ()
import           Data.Set (Set(..))
import qualified Data.Set as S
import           Data.String
import           Data.Text (Text)
import           Data.Typeable ()
import           Database.PostgreSQL.Simple hiding (fold)
import           Database.PostgreSQL.Simple.Expr
import           Database.PostgreSQL.Simple.FromRow
import           Fake
import           GHC.Generics

commas :: (IsString a, Monoid a) => [a] -> a
commas = mconcat . intersperse ", "

class HasTable a => FakeRows a where
  populate :: MonadConnection m => Int -> m ()

  default populate :: forall m key. (MonadConnection m,
                       HasTable a, ToRow a, HasKey a, Generic a,
                       GFake (Rep a), GGetFKs (Rep a), Key a ~ key,
                       Fake key, KeyField key, Ord key, ToDynMap key)
                      => Int -> m ()
  populate numRows = genericPopulate @m @a numRows

genericPopulate :: forall m a key.
                   (MonadConnection m,
                    HasTable a, ToRow a, HasKey a, Generic a,
                    GFake (Rep a), GGetFKs (Rep a), Key a ~ key,
                    Fake key, KeyField key, Ord key, ToDynMap key)
                => Int -> m ()
genericPopulate n = do
  rows <- generateRows @m @a n
  traverse_ insertAll rows

genericPopulateNoKey :: forall m a.
  (MonadConnection m, HasTable a, ToRow a, Generic a,
    GFake (Rep a), GGetFKs (Rep a))
  => Int -> m ()
genericPopulateNoKey n = do
  fkMaps <- getRandomFKs @m @a n []
  rows <- liftIO . generate $ traverse (gFGenWith @a) fkMaps
  traverse_ insertAll rows

generateRows :: forall m a key.
  (MonadConnection m, HasTable a, ToRow a, HasKey a, Generic a,
   GFake (Rep a), GGetFKs (Rep a), Key a ~ key,
   Fake key, KeyField key, Ord key, ToDynMap key)
  => Int -> m [a]
generateRows n = do
  let keyFlds = getKeyFieldNames (Proxy :: Proxy a)

  -- generate n unique tuples for key
  keys :: Set key <- liftIO . generate $ generateUniqueKeys @a n

  -- turn keys into map of keyFieldName --> keyValue.
  let keyMaps = map (toDynMap keyFlds) (S.toAscList keys)

  -- generate foreign keys as maps of fkFieldName --> Foreign (Key parent)
  fkMaps <- getRandomFKs @m @a n keyFlds

  -- merge key maps and foreign key maps, with the FKs taking precedence
  let maps = zipWith (<>) fkMaps keyMaps

  -- gen n fake rows of A, using keymaps for each key
  liftIO . generate $ traverse (gFGenWith @a) maps

------------------------------------------------------------

-- | Returns n maps of foreign keys, where each map contains an entry
-- for each Foreign type in the table of type `a`.
-- The key is the haskell field name of the fk as the key
-- The value is the (Key p) type where p is the parent table
getRandomFKs :: forall m a.
  (MonadConnection m, HasTable a, Generic a, GGetFKs (Rep a))
  => Int -> [String] -> m [Map String Dynamic]
getRandomFKs n keyFlds = gGetRandomFKs @(Rep a) n keyFlds

class GGetFKs f where
  gGetRandomFKs :: (MonadConnection m)
                => Int -> [String] -> m [Map String Dynamic]

instance GGetFKs f => GGetFKs (M1 D c f) where
  gGetRandomFKs = gGetRandomFKs @f

instance GGetFKs f => GGetFKs (M1 C c f) where
  gGetRandomFKs = gGetRandomFKs @f

instance {-# OVERLAPS #-} (HasTable t, HasKey t, FromRow t, KeyField (Key t),
                           Typeable (Foreign t), Selector c) =>
  GGetFKs (M1 S c (K1 R (Foreign t))) where
  gGetRandomFKs :: forall m. (MonadConnection m)
                => Int -> [String] -> m [Map String Dynamic]
  gGetRandomFKs n keyFlds =
    let sel = selName $ (undefined :: M1 S c (K1 R (Foreign t)) ())
    in selectFKsAsMap @m @t n sel (sel `elem` keyFlds)

instance {-# OVERLAPS #-} (HasTable t, HasKey t, KeyField (Key t), FromRow t,
                           Typeable (Maybe (Foreign t)), Selector c) =>
  GGetFKs (M1 S c (K1 R (Maybe (Foreign t)))) where
  gGetRandomFKs :: forall m. (MonadConnection m)
                => Int -> [String] -> m [Map String Dynamic]
  gGetRandomFKs n keyFlds =
    let sel = selName $ (undefined :: M1 S c (K1 R (Maybe (Foreign t) )) ())
    in do
      keys <- selectFKs @m @t n (sel `elem` keyFlds)
      let toMap k = liftIO $ do
            usefk <- generate fakeEnum
            let v = if usefk then Just (Foreign k :: Foreign t) else Nothing
            return $ M.insert sel (toDyn v) M.empty
      traverse toMap keys

instance {-# OVERLAPPABLE #-} GGetFKs (M1 S c (K1 R t)) where
  gGetRandomFKs n _ = pure $ replicate n M.empty

instance (GGetFKs f, GGetFKs g) => GGetFKs (f :*: g) where
  gGetRandomFKs n kfs = do
    m1 <- gGetRandomFKs @f n kfs
    m2 <- gGetRandomFKs @g n kfs
    return $ zipWith (<>) m1 m2

selectFKsAsMap :: forall m parent.
  (MonadConnection m, HasKey parent, FromRow parent,
   KeyField (Key parent), Typeable (Foreign parent))
  => Int -> String -> Bool -> m [Map String Dynamic]
selectFKsAsMap n childFld isPartOfKey = do
  keys <- selectFKs @m @parent n isPartOfKey
  let toMap k = M.insert childFld (toDyn (Foreign k :: Foreign parent)) M.empty
  return $ map toMap keys

newtype Random t = Random { unRandom :: (Double, t) }
instance (FromRow t) => FromRow (Random t) where
  fromRow = do
    r <- field
    t <- fromRow
    return $ Random (r, t)

selectFKs :: forall m parent.
  (MonadConnection m, HasTable parent, HasKey parent,
   FromRow parent, KeyField (Key parent))
  => Int -> Bool -> m [Key parent]
selectFKs n isPartOfKey = do
  let
    unique = if isPartOfKey then " distinct " else ""
    kflds = getFieldNames (Proxy :: Proxy parent)
    tbl = tableName (Proxy :: Proxy parent)
    q = "select " <> unique <> " random() as r, "<> commas kflds
      <> " from "<> tbl <> " order by r limit " <> show n
  ks :: [Random parent] <- queryC (fromString q) ()
  return $ map (getKey @parent . snd . unRandom) ks

------------------------------------------------------------

generateUniqueKeys ::
  forall a. (HasFieldNames a, HasKey a, Ord (Key a), Fake (Key a))
  => Int -> FGen (Set (Key a))
generateUniqueKeys n =
  genKeys 0 S.empty
  where
    genKeys :: Int -> Set (Key a) -> FGen (Set (Key a))
    genKeys i res | i > n^2 = uniqueKeyError i (S.size res)
                  | otherwise =
                      if S.size res >= n
                      then return res
                      else do
                        k <- fake @(Key a)
                        -- k <- gFGen @(Key a)
                        genKeys (i+1) (S.insert k res)
    uniqueKeyError i sz = error $
      mconcat ["Could not generate ",show n," fakes for key with fields "
              , commas (getKeyFieldNames (Proxy :: Proxy  a))
              , " in the table with field names: "
              , commas (getFieldNames (Proxy :: Proxy a))
              , "\nCheck the fake instance for the corresponding data type."
              ,"\nError was called after ",show i
              , " iterations, after generating "<>show sz<> " unique keys"]

class ToDynMap key where
  toDynMap :: [String] -> key -> Map String Dynamic

instance {-# OVERLAPS #-} ToDynMap Int where
  toDynMap (fld:_) v = M.fromList [(fld, toDyn v)]
  toDynMap _ v = M.fromList [("__unknown__", toDyn v)]

instance {-# OVERLAPS #-} ToDynMap Float where
  toDynMap (fld:_) v = M.fromList [(fld, toDyn v)]
  toDynMap _ v = M.fromList [("__unknown__", toDyn v)]

instance {-# OVERLAPS #-} ToDynMap Double where
  toDynMap (fld:_) v = M.fromList [(fld, toDyn v)]
  toDynMap _ v = M.fromList [("__unknown__", toDyn v)]

instance {-# OVERLAPS #-} ToDynMap Char where
  toDynMap (fld:_) v = M.fromList [(fld, toDyn v)]
  toDynMap _ v = M.fromList [("__unknown__", toDyn v)]

instance {-# OVERLAPS #-} ToDynMap Text where
  toDynMap (fld:_) v = M.fromList [(fld, toDyn v)]

instance {-# OVERLAPS #-} (Generic key, Typeable key, GToDynMap (Rep key)) => ToDynMap key where
  toDynMap = gToDynMap

gToDynMap :: (Generic a, GToDynMap (Rep a), Typeable a)
         => [String] -> a -> Map String Dynamic
gToDynMap [x] k = M.fromList [(x, toDyn k)]
gToDynMap keyFlds k = M.fromList . gtoDynMap keyFlds $ from k

class GToDynMap f where
  gtoDynMap :: [String] -> f p -> [(String, Dynamic)]

instance (Typeable t, Selector d) => GToDynMap (M1 S d (K1 R t)) where
  gtoDynMap (fld:_) (M1 (K1 x)) = [(fld, toDyn x)]

instance GToDynMap f => GToDynMap (M1 D c f) where
  gtoDynMap flds (M1 f) = gtoDynMap flds f

instance GToDynMap f => GToDynMap (M1 C c f) where
  gtoDynMap flds (M1 f) = gtoDynMap flds f

instance (GToDynMap f, GToDynMap g) => GToDynMap (f :*: g) where
  gtoDynMap (fldF:flds) (f :*: g) = gtoDynMap [fldF] f ++ gtoDynMap flds g

------------------------------------------------------------

gFGen :: (Generic a, GFake (Rep a)) => FGen a
gFGen = gFGenWith M.empty

gFGenWith :: forall a. (Generic a, GFake (Rep a))
          => Map String Dynamic -> FGen a
gFGenWith m = to <$> gfakeWith @(Rep a) m

-- | Generic fake data generator, with optional map of selector names
-- to values which will be used instead of a fake.
class GFake f where
  gfakeWith :: Map String Dynamic -> FGen (f p)

gset :: forall a d p. (Show a, Typeable a, Fake a)
     => String -> Map String Dynamic -> FGen (M1 S d (K1 R a) p)
gset sel m =
  case M.lookup sel m >>= fromDynamic @a of
    Nothing -> (M1 . K1) <$> fake
    Just v -> return $ M1 (K1 v)

instance (Selector d, Typeable a, Fake a) => GFake (M1 S d (K1 R a)) where
  gfakeWith m =
    let sel = (selName (undefined :: M1 S d (K1 R a) ()))
    in case M.lookup sel m >>= fromDynamic @a of
      Nothing -> M1 . K1 <$> fake
      Just v -> pure $ M1 (K1 v)

instance GFake f => GFake (M1 C c f) where
  gfakeWith m = M1 <$> gfakeWith @f m

instance GFake f => GFake (M1 D c f) where
  gfakeWith m = M1 <$> gfakeWith @f m

instance (GFake a, GFake b) => GFake (a :*: b) where
  gfakeWith m = liftA2 (:*:)
            (gfakeWith @a m)
            (gfakeWith @b m)

instance (GFake a, GFake b) => GFake (a :+: b) where
  gfakeWith m = do
    bool <- fakeEnum
    if bool
      then fmap L1 $ gfakeWith @a m
      else fmap R1 $ gfakeWith @b m
