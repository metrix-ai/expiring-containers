module ExpiringContainers.ExpiringMap
(
  ExpiringMap,

  -- * Construction
  empty,
  singleton,

  -- * List
  toList,
  fromList,

  -- * Transformations
  map,
  mapWithKey,
  traverseWithKey,

  -- * Basic interface
  null,
  size,
  member,
  insert,
  insertIfNotOlder,
  delete,
  lookupWithTime,
  lookup,

  -- * Filter
  deleteEntriesBefore,
)
where

import qualified ExpiringContainers.ExpiringSet as ExpiringSet
import qualified Data.HashMap.Strict as HashMap
import Data.Time
import Data.Maybe
import Prelude hiding (lookup, null, map)
import Data.Hashable
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified GHC.Exts as G

{-|

-}
data ExpiringMap key value =
  ExpiringMap
    (ExpiringSet.ExpiringSet key)
    (HashMap.HashMap key value)
    deriving (Eq, Foldable, Show)

instance Functor (ExpiringMap key) where
  fmap = map

instance Traversable (ExpiringMap key) where
  traverse f = traverseWithKey (const f)

{--------------------------------------------------------------------
  Transformations
--------------------------------------------------------------------}
map :: (v1 -> v2) -> ExpiringMap k v1 -> ExpiringMap k v2
map f = mapWithKey (const f)
{-# INLINE map #-}

mapWithKey :: (k -> v1 -> v2) -> ExpiringMap k v1 -> ExpiringMap k v2
mapWithKey f (ExpiringMap expiringSet hashMap) =
  ExpiringMap expiringSet $ HashMap.mapWithKey f hashMap

traverseWithKey :: Applicative f => (k -> v1 -> f v2) -> ExpiringMap k v1
  -> f (ExpiringMap k v2)
traverseWithKey f (ExpiringMap expiringSet hashMap) = ExpiringMap expiringSet <$> HashMap.traverseWithKey f hashMap

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
instance (Eq a, Hashable a) => G.IsList (ExpiringMap a b) where
  type Item (ExpiringMap a b) = (UTCTime, a, b)
  toList = toList
  fromList = fromList

toList :: (Eq k, Hashable k) => ExpiringMap k v -> [(UTCTime, k, v)]
toList (ExpiringMap expiringSet hashMap) =
  fmap (\(time, key) -> (time, key, hashMap HashMap.! key)) $ ExpiringSet.toList expiringSet

fromList :: (Eq k, Hashable k) =>
     [(UTCTime, k, v)] -> ExpiringMap k v
fromList list = ExpiringMap expSet hashMap
  where
    (expSetList, hashMapList) = Foldable.foldl' (\(xs, ys) (t,k,v) -> ((t,k) : xs, (k,v) : ys)) ([], []) list
    expSet = ExpiringSet.fromList expSetList
    hashMap = HashMap.fromList hashMapList

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
empty :: (Eq k, Hashable k) => ExpiringMap k v
empty = ExpiringMap ExpiringSet.empty HashMap.empty

singleton :: (Eq k, Hashable k) => UTCTime -> k -> v -> ExpiringMap k v
singleton time k v = ExpiringMap (ExpiringSet.singleton time k) (HashMap.singleton k v)
{-# INLINABLE singleton #-}

{--------------------------------------------------------------------
  Basic interface
--------------------------------------------------------------------}
null :: ExpiringMap k v -> Bool
null (ExpiringMap _ hashMap) = HashMap.null hashMap
{-# INLINE null #-}

size :: ExpiringMap k v -> Int
size (ExpiringMap _ hashMap) = HashMap.size hashMap

member :: (Eq k, Hashable k) => k -> ExpiringMap k v -> Bool
member key (ExpiringMap _ hashMap) = HashMap.member key hashMap

{-|
Associate the specified value with the specified key and time in this map.
If this map previously contained a mapping for the key and the time, the old values is replaced.
-}
insert :: (Eq k,  Hashable k) => UTCTime {-^ Expiry time -} -> k -> v -> ExpiringMap k v -> ExpiringMap k v
insert time key value (ExpiringMap expSet hashMap) =
  ExpiringMap (ExpiringSet.insert time key expSet) (HashMap.insert key value hashMap)

{-|
Associate the specified value with the specified key and time in this map.
Of this map previously contained a mapping for the key and the time; 
the old values is replaced only if the previous time is older than the new one. 
-}
insertIfNotOlder :: (Eq k,  Hashable k) => UTCTime {-^ Expiry time -} -> k -> v -> ExpiringMap k v -> ExpiringMap k v
insertIfNotOlder time key value expMap@(ExpiringMap expSet hashMap) =
  let ifOlder = case (ExpiringSet.lookup key expSet) of
        Nothing -> False
        Just t -> if time < t then True else False
  in if ifOlder 
    then expMap 
    else (ExpiringMap (ExpiringSet.insert time key expSet) (HashMap.insert key value hashMap))
      
delete :: (Eq k, Hashable k) => k -> ExpiringMap k v -> ExpiringMap k v
delete key (ExpiringMap expSet hashMap) =
  ExpiringMap (ExpiringSet.delete key expSet) (HashMap.delete key hashMap)

lookup :: (Eq k, Hashable k) => k -> ExpiringMap k v -> Maybe v
lookup key (ExpiringMap expSet hashMap) =
  HashMap.lookup key hashMap

lookupWithTime :: (Eq k, Hashable k) => k -> ExpiringMap k v -> Maybe (v, UTCTime)
lookupWithTime key (ExpiringMap expSet hashMap) =
  HashMap.lookup key hashMap >>= (\v -> fmap ((,) v) $ ExpiringSet.lookup key expSet)

{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
deleteEntriesBefore :: (Eq k, Hashable k) => UTCTime -> ExpiringMap k v -> ([(k, v)], ExpiringMap k v)
deleteEntriesBefore time (ExpiringMap expSet hashMap) =
  (listElem, ExpiringMap newExpSet newHashMap)
    where
      (keys, newExpSet) = ExpiringSet.deleteEntriesBefore time expSet
      newHashMap = List.foldl' (flip HashMap.delete) hashMap keys
      listElem = fmap (\k -> (k, hashMap HashMap.! k)) keys
