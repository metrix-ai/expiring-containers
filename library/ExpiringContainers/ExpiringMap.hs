module ExpiringContainers.ExpiringMap
(
  ExpiringMap,
  lookup,
  setCurrentTime,

  -- * Construction
  empty,
  singleton,

  -- * List
  toList,
  fromList,

  -- * Transformations
  map,
  mapWithKey,

  -- * Basic interface
  null,
  size,
  member,
  insert,
  delete,
)
where

import qualified ExpiringContainers.ExpiringSet as A
import qualified Data.HashMap.Strict as B
import Data.Time
import Data.Maybe
import Prelude hiding (lookup, null, map)
import Data.Hashable
import qualified Data.Foldable as D
import qualified Data.List as C
import qualified GHC.Exts as G

{-|

-}
data ExpiringMap key value =
  ExpiringMap
    (A.ExpiringSet key)
    (B.HashMap key value)
    deriving (Eq, Foldable, Show)

{--------------------------------------------------------------------
  Transformations
--------------------------------------------------------------------}
map :: (v1 -> v2) -> ExpiringMap k v1 -> ExpiringMap k v2
map f = mapWithKey (const f)
{-# INLINE map #-}

mapWithKey :: (k -> v1 -> v2) -> ExpiringMap k v1 -> ExpiringMap k v2
mapWithKey f (ExpiringMap expiringSet hashMap) =
  ExpiringMap expiringSet $ B.mapWithKey f hashMap

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
instance (Eq a, Hashable a) => G.IsList (ExpiringMap a b) where
  type Item (ExpiringMap a b) = (UTCTime, a, b)
  toList = toList
  fromList = fromList

toList :: (Eq k, Hashable k) => ExpiringMap k v -> [(UTCTime, k, v)]
toList (ExpiringMap expiringSet hashMap) =
  fmap (\(time, key) -> (time, key, hashMap B.! key)) $ A.toList expiringSet

fromList :: (Eq k, Hashable k) =>
     [(UTCTime, k, v)] -> ExpiringMap k v
fromList list = ExpiringMap expSet hashMap
  where
    (expSetList, hashMapList) = D.foldl' (\(xs, ys) (t,k,v) -> ((t,k) : xs, (k,v) : ys)) ([], []) list
    expSet = A.fromList expSetList
    hashMap = B.fromList hashMapList

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
empty :: (Eq k, Hashable k) => ExpiringMap k v
empty = ExpiringMap A.empty B.empty

singleton :: (Eq k, Hashable k) => UTCTime -> k -> v -> ExpiringMap k v
singleton time k v = ExpiringMap (A.singleton time k) (B.singleton k v)
{-# INLINABLE singleton #-}

{--------------------------------------------------------------------
  Basic interface
--------------------------------------------------------------------}
null :: ExpiringMap k v -> Bool
null (ExpiringMap _ hashMap) = B.null hashMap
{-# INLINE null #-}

size :: ExpiringMap k v -> Int
size (ExpiringMap _ hashMap) = B.size hashMap

member :: (Eq k, Hashable k) => k -> ExpiringMap k v -> Bool
member key (ExpiringMap _ hashMap) = B.member key hashMap

insert :: (Eq k,  Hashable k) => UTCTime {-^ Expiry time -} -> k -> v -> ExpiringMap k v -> ExpiringMap k v
insert time key value (ExpiringMap expSet hashMap) =
  ExpiringMap (A.insert time key expSet) (B.insert key value hashMap)

delete :: (Eq k, Hashable k) => k -> ExpiringMap k v -> ExpiringMap k v
delete key (ExpiringMap expSet hashMap) =
  ExpiringMap (A.delete key expSet) (B.delete key hashMap)

lookup :: (Eq k, Hashable k) => k -> ExpiringMap k v -> Maybe v
lookup key (ExpiringMap expSet hashMap) =
  B.lookup key hashMap

setCurrentTime :: (Eq k, Hashable k) => UTCTime -> ExpiringMap k v -> ExpiringMap k v
setCurrentTime time (ExpiringMap expSet hashMap) =
  ExpiringMap newExpSet newHashMap
    where
      (keys, newExpSet) = A.clean time expSet
      newHashMap = C.foldl' (flip B.delete) hashMap keys
