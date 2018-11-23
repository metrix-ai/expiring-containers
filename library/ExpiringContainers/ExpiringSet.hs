module ExpiringContainers.ExpiringSet
(
  ExpiringSet,
  -- * Construction
  empty,
  singleton,

  -- * List
  toList,
  fromList,

  -- * Transformations
  map,

  -- * Basic interface
  null,
  insert,
  insertForce,
  delete,
  member,
  memberTime,
  size,

  -- * Filter
  clean,
)
where

import qualified Data.HashMap.Strict as HashMap
import qualified IntMultimap as IntMap
import qualified Data.Foldable as Foldable
import qualified GHC.Exts as G
import Data.Time
import Data.Int
import Prelude hiding(map, null)
import GHC.Generics
import Timestamp
import Data.Hashable
import Control.Arrow

{-|
Set that expiring with time
-}
data ExpiringSet element =
  ExpiringSet
    {-| Elements indexed by timestamps -}
    (IntMap.IntMultimap element)
    {-| Timestamps indexed by elements -}
    (HashMap.HashMap element Int)
    deriving(Eq, Show, Generic)



{--------------------------------------------------------------------
  Transformations
--------------------------------------------------------------------}
map :: (Eq b, Hashable b) => (a -> b) -> ExpiringSet a -> ExpiringSet b
map f (ExpiringSet intMultimap hashMap) = uncurry ExpiringSet $ IntMap.foldlWithKey' step (IntMap.empty, HashMap.empty) intMultimap where
  step stamp (!intMultimap', !hashMap') x
    | Just v <- HashMap.lookup y hashMap' = (IntMap.insert stamp y $ IntMap.delete v y intMultimap', HashMap.insert y stamp hashMap')
    | otherwise = (IntMap.insert stamp y intMultimap', HashMap.insert y stamp hashMap')
    where y = f x
{-# INLINE map #-}

construct :: (Eq k, Hashable k) => HashMap.HashMap k Int -> ExpiringSet k
construct hashMap = ExpiringSet intMultimap hashMap
  where
    intMultimap = hashToMap hashMap

hashToMap :: (Eq a, Hashable a) => HashMap.HashMap a Int -> IntMap.IntMultimap a
hashToMap hashMap =
  HashMap.foldlWithKey' (\intMultiMap key value -> IntMap.insert value key intMultiMap) IntMap.empty hashMap

mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap.HashMap k1 a -> HashMap.HashMap k2 a
mapKeys f hashMap = HashMap.fromList $ fmap (first f) $ (HashMap.toList hashMap)


{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
instance (Eq a, Hashable a) => G.IsList (ExpiringSet a) where
  type Item (ExpiringSet a) = (UTCTime, a)
  toList = toList
  fromList = fromList

toList :: ExpiringSet a -> [(UTCTime, a)]
toList (ExpiringSet intMultiMap _) = fmap (\(k, a) -> (,) (timestampUtcTime $ Timestamp $ fromIntegral k) a) $ IntMap.toList intMultiMap

fromList :: (Eq a, Hashable a) =>
     [(UTCTime, a)] -> ExpiringSet a
fromList = construct . HashMap.fromList . fmap (\(t, a) -> (,) a (fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) t))

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
empty :: (Eq a, Hashable a) => ExpiringSet a
empty = ExpiringSet IntMap.empty HashMap.empty

singleton :: (Eq a, Hashable a) => UTCTime -> a -> ExpiringSet a
singleton time v = construct $ HashMap.singleton v key
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time
{-# INLINABLE singleton #-}


{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
{-|
Clean expiringset
-}
clean :: (Hashable element, Eq element) => UTCTime -> ExpiringSet element -> ([element], ExpiringSet element)
clean time (ExpiringSet intMultiMap hashMap) =
  (listElem, ExpiringSet newMultiMap newHash)
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time
    newHash = HashMap.filterWithKey (\_ k -> k >= key) hashMap
    (oldMultiMap, maybeElem, newMultiMap) = (IntMap.splitLookup key intMultiMap)
    element = case maybeElem of
      Just a -> Foldable.toList a
      Nothing -> []
    listElem = (Foldable.toList oldMultiMap) ++ element

{--------------------------------------------------------------------
  Basic interface
--------------------------------------------------------------------}
null :: ExpiringSet a -> Bool
null (ExpiringSet _ hashMap) = HashMap.null hashMap
{-# INLINE null #-}

size :: ExpiringSet a -> Int
size (ExpiringSet _ hashMap) = HashMap.size hashMap

member :: (Eq a, Hashable a) => a -> ExpiringSet a -> Bool
member a (ExpiringSet _ hashMap) = HashMap.member a hashMap

memberTime :: UTCTime -> ExpiringSet a -> Bool
memberTime time (ExpiringSet intMultiMap _) = IntMap.member key intMultiMap
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time

insertForce :: (Hashable element, Eq element) => UTCTime {-^ Expiry time -} -> element -> ExpiringSet element -> ExpiringSet element
insertForce time value (ExpiringSet intMultiMap hashMap) =
  ExpiringSet newMultiMap (HashMap.insert value key hashMap)
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time
    maybeTimestamp = HashMap.lookup value hashMap
    newMultiMap = case maybeTimestamp of
      Nothing -> IntMap.insert key value intMultiMap
      Just k -> IntMap.insert key value $ IntMap.delete k value intMultiMap

{-|

-}
insert :: (Hashable element, Eq element) => UTCTime {-^ Expiry time -} -> element -> ExpiringSet element -> ExpiringSet element
insert time value (ExpiringSet intMultiMap hashMap) =
  ExpiringSet newMultiMap newHash
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time
    maybeTimestamp = HashMap.lookup value hashMap
    (newMultiMap, newHash) = case maybeTimestamp of
      Nothing -> (IntMap.insert key value intMultiMap, HashMap.insert value key hashMap)
      Just k -> if key >= k
        then (IntMap.insert key value $ IntMap.delete k value intMultiMap, HashMap.insert value key hashMap)
        else (intMultiMap, hashMap)

deleteByTime :: (Hashable element, Eq element) => UTCTime {-^ Expiry time -} -> element -> ExpiringSet element -> ExpiringSet element
deleteByTime time element (ExpiringSet _ hashMap) =
  construct $ HashMap.delete element hashMap
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time

delete :: (Hashable element, Eq element) => element -> ExpiringSet element -> ExpiringSet element
delete value (ExpiringSet intMultiMap hashMap) =
  ExpiringSet newMultiMap (HashMap.delete value hashMap)
  where
    maybeKey = HashMap.lookup value hashMap
    newMultiMap = case maybeKey of
      Nothing -> intMultiMap
      Just key -> IntMap.delete key value intMultiMap
