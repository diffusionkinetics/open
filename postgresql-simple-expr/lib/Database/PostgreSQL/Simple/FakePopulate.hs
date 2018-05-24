{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric,
             DefaultSignatures, PolyKinds, TypeOperators, ScopedTypeVariables,
             FlexibleContexts, FlexibleInstances, OverloadedStrings,
             TypeApplications, AllowAmbiguousTypes #-}
module Database.PostgreSQL.Simple.FakePopulate
  ( FakeRows(..), gFGenWith, gtoDynMap, generateUniqueKeys, toKeyMap, getForeignKeys, getFKs, getFKMap, generateRows
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Dynamic
import           Data.Foldable
import           Data.List (intersperse, find)
import           Data.Map (Map(..))
import qualified Data.Map as M
import           Data.Monoid ((<>), mconcat)
import           Data.Proxy
import           Data.Set (Set(..))
import qualified Data.Set as S
import           Data.String
import           Data.Typeable ()
import           Database.PostgreSQL.Simple hiding (fold)
import           Database.PostgreSQL.Simple.Expr
import           Database.PostgreSQL.Simple.FromRow
import           Fake
import           GHC.Generics

_1 :: (a,b,c) -> a
_1 (x,_,_) = x

_2 :: (a,b,c) -> b
_2 (_,x,_) = x

_3 :: (a,b,c) -> c
_3 (_,_,x) = x

commas :: (IsString a, Monoid a) => [a] -> a
commas = mconcat . intersperse ", "

toQs = commas . map (const "?")

------------------------------------------------------------

-- | (name of referencing column, referenced table, referenced column)
type ForeignKey = (String, String, String)

class HasTable a => FakeRows a where
  foreignKeys :: [ForeignKey]
  foreignKeys = []
  numRows :: Int
  numRows = 100
  populate :: MonadConnection m => m ()

  default populate :: forall m key. (MonadConnection m,
                       HasTable a, ToRow a, HasKey a, Generic a,
                       GFake (Rep a), GGetFKs (Rep a),
                       Key a ~ key, Fake key, KeyField key, Ord key, Generic key,
                       Typeable key, GFake (Rep key), GToDynMap (Rep key))
                      => m ()
  populate = genericPopulate @m @a (numRows @a) (foreignKeys @a)

genericPopulate :: forall m a key.
                   (MonadConnection m,
                    HasTable a, ToRow a, HasKey a, Generic a,
                    GFake (Rep a), GGetFKs (Rep a),
                    Key a ~ key, Fake key, KeyField key, Ord key, Generic key,
                    Typeable key, GFake (Rep key), GToDynMap (Rep key))
                => Int -> [ForeignKey] -> m ()
genericPopulate n fks = do
  rows <- generateRows @m @a n fks
  traverse_ insertAll rows

generateRows :: forall m a key.
  (MonadConnection m, HasTable a, ToRow a, HasKey a, Generic a,
   GFake (Rep a), GGetFKs (Rep a),
   Key a ~ key, Fake key, KeyField key, Ord key, Generic key,
   Typeable key, GFake (Rep key), GToDynMap (Rep key))
  => Int -> [ForeignKey] -> m [a]
generateRows n fks = do
  let keyFlds = getKeyFieldNames (Proxy :: Proxy a)

  -- generate n unique tuples for key
  keys :: Set key <- liftIO . generate $ generateUniqueKeys @a n

  -- turn keys into map of keyFieldName --> keyValue.
  let keyMaps = map (toKeyMap keyFlds) (S.toAscList keys)

  -- generate foreign keys as maps of fkFieldName --> Foreign (Key parent)
  fkMaps <- getForeignKeys @m @a n fks

  -- merge key maps and foreign key maps, with the FKs taking precedence
  let maps = zipWith (<>) fkMaps keyMaps

  -- gen n fake rows of A, using keymaps for each key
  liftIO . generate $ traverse (gFGenWith @a) maps

-- | Returns n maps of foreign keys, where each map contains an entry
-- for each Foreign type in the table of type `a`.
-- The key is the haskell field name of the fk as the key
-- The value is the (Key p) type where p is the parent table
getForeignKeys :: forall m a. (MonadConnection m, HasTable a, HasKey a, Generic a, GGetFKs (Rep a))
               => Int -> [ForeignKey] -> m [Map String Dynamic]
getForeignKeys n [] = return $ replicate n M.empty
getForeignKeys n fks =
  let keyFlds = getKeyFieldNames (Proxy :: Proxy a)
      fks' = map (\fk@(childFld,_,_) -> (fk, childFld `elem` keyFlds)) fks
  in ggetFKs (Proxy :: Proxy (Rep a)) n fks'

class GGetFKs f where
  ggetFKs :: (MonadConnection m) => Proxy f -> Int -> [(ForeignKey, Bool)] -> m [Map String Dynamic]

instance GGetFKs f => GGetFKs (M1 D c f) where
  ggetFKs _ = ggetFKs (undefined :: Proxy f)

instance GGetFKs f => GGetFKs (M1 C c f) where
  ggetFKs _ = ggetFKs (undefined :: Proxy f)

instance {-# OVERLAPS #-} (HasTable t, HasKey t, FromRow t, KeyField (Key t), Typeable (Foreign t), Selector c) => GGetFKs (M1 S c (K1 R (Foreign t))) where
  ggetFKs _ n fks =
    let
      sel = selName $ (undefined :: M1 S c (K1 R (Foreign t)) ())
      tbl = tableName (Proxy :: Proxy t)
      found = find (\((childFld, parTbl,_), pok) ->
                      sel == childFld && parTbl == tbl) fks
    in case found of
      Nothing -> pure $ replicate n M.empty
      Just (fk, pok) -> getFKMap (Proxy :: Proxy t) n pok fk

instance {-# OVERLAPPABLE #-} GGetFKs (M1 S c (K1 R t)) where
  ggetFKs _ n _ = pure $ replicate n M.empty

instance (GGetFKs f, GGetFKs g) => GGetFKs (f :*: g) where
  ggetFKs _ n fks = do
    m1 <- ggetFKs (Proxy :: Proxy f) n fks
    m2 <- ggetFKs (Proxy :: Proxy g) n fks
    return $ zipWith (<>) m1 m2

getFKMap :: forall m parent.
  (MonadConnection m, HasKey parent, FromRow parent, KeyField (Key parent), Typeable (Foreign parent))
  => Proxy parent -> Int -> Bool -> ForeignKey -> m [Map String Dynamic]
getFKMap _ n isPartOfKey fk@(childFld, parentTbl, parentFld) = do
  keys <- getFKs @m @parent n isPartOfKey fk
  return $ map (\k -> M.insert childFld (toDyn (Foreign k :: Foreign parent)) M.empty) keys

newtype Random t = Random { unRandom :: (Double, t) }
instance (FromRow t) => FromRow (Random t) where
  fromRow = do
    r <- field
    t <- fromRow
    return $ Random (r, t)

getFKs :: forall m parent.
  (MonadConnection m, HasKey parent, FromRow parent, KeyField (Key parent))
  => Int -> Bool -> ForeignKey -> m [Key parent]
getFKs n isPartOfKey (_, parentTbl, _) = do
  let
    unique = if isPartOfKey then " distinct " else ""
    kflds = getFieldNames (Proxy :: Proxy parent)
    q = "select " <> unique <> " random() as r, "<>commas kflds <> " from "<> parentTbl <> " order by r limit " <> show n
  ks :: [Random parent] <- queryC (fromString q) ()
  return $ map (getKey @parent . snd . unRandom) ks

------------------------------------------------------------

generateUniqueKeys :: forall a.
                      (HasFieldNames a, HasKey a,
                       -- Ord (Key a), Generic (Key a), GFake (Rep (Key a)))
                       Ord (Key a), Generic (Key a), Fake (Key a))
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

toKeyMap :: (Generic a, GToDynMap (Rep a), Typeable a)
         => [String] -> a -> Map String Dynamic
toKeyMap [x] k = M.fromList [(x, toDyn k)]
toKeyMap keyFlds k = gtoDynMap keyFlds k

gtoDynMap :: (Generic a, GToDynMap (Rep a)) => [String] -> a -> Map String Dynamic
gtoDynMap flds = M.fromList . gToMap flds . from

class GToDynMap f where
  gToMap :: [String] -> f p -> [(String, Dynamic)]

instance (Typeable t, Selector d) => GToDynMap (M1 S d (K1 R t)) where
  gToMap (fld:_) (M1 (K1 x)) = [(fld, toDyn x)]

instance GToDynMap f => GToDynMap (M1 D c f) where
  gToMap flds (M1 f) = gToMap flds f

instance GToDynMap f => GToDynMap (M1 C c f) where
  gToMap flds (M1 f) = gToMap flds f

instance (GToDynMap f, GToDynMap g) => GToDynMap (f :*: g) where
  gToMap (fldF:flds) (f :*: g) = gToMap [fldF] f ++ gToMap flds g

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
