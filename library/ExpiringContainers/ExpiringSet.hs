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

import qualified Data.HashMap.Strict as A
import qualified IntMultimap as B
import qualified Data.HashSet as C
import qualified Data.Foldable as D
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
    (B.IntMultimap element)
    {-| Timestamps indexed by elements -}
    (A.HashMap element Int)
    deriving(Eq, Show, Generic)



{--------------------------------------------------------------------
  Transformations
--------------------------------------------------------------------}
map :: (Eq b, Hashable b) => (a -> b) -> ExpiringSet a -> ExpiringSet b
map f (ExpiringSet intMultimap hashMap) = uncurry ExpiringSet $ B.foldlWithKey' step (B.empty, A.empty) intMultimap where
  step stamp (!intMultimap', !hashMap') x
    | Just v <- A.lookup y hashMap' = (B.insert stamp y $ B.delete v y intMultimap', A.insert y stamp hashMap')
    | otherwise = (B.insert stamp y intMultimap', A.insert y stamp hashMap')
    where y = f x
{-# INLINE map #-}

construct :: (Eq k, Hashable k) => A.HashMap k Int -> ExpiringSet k
construct hashMap = ExpiringSet intMultimap hashMap
  where
    intMultimap = hashToMap hashMap

hashToMap :: (Eq a, Hashable a) => A.HashMap a Int -> B.IntMultimap a
hashToMap hashMap =
  A.foldlWithKey' (\intMultiMap key value -> B.insert value key intMultiMap) B.empty hashMap

mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> A.HashMap k1 a -> A.HashMap k2 a
mapKeys f hashMap = A.fromList $ fmap (first f) $ (A.toList hashMap)


{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
instance (Eq a, Hashable a) => G.IsList (ExpiringSet a) where
  type Item (ExpiringSet a) = (UTCTime, a)
  toList = toList
  fromList = fromList

toList :: ExpiringSet a -> [(UTCTime, a)]
toList (ExpiringSet intMultiMap _) = fmap (\(k, a) -> (,) (timestampUtcTime $ Timestamp $ fromIntegral k) a) $ B.toList intMultiMap

fromList :: (Eq a, Hashable a) =>
     [(UTCTime, a)] -> ExpiringSet a
fromList = construct . A.fromList . fmap (\(t, a) -> (,) a (fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) t))

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
empty :: (Eq a, Hashable a) => ExpiringSet a
empty = ExpiringSet B.empty A.empty

singleton :: (Eq a, Hashable a) => UTCTime -> a -> ExpiringSet a
singleton time v = construct $ A.singleton v key
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
    newHash = A.filterWithKey (\_ k -> k >= key) hashMap
    (oldMultiMap, maybeElem, newMultiMap) = (B.splitLookup key intMultiMap)
    element = case maybeElem of
      Just a -> D.toList a
      Nothing -> []
    listElem = (D.toList oldMultiMap) ++ element

{--------------------------------------------------------------------
  Basic interface
--------------------------------------------------------------------}
null :: ExpiringSet a -> Bool
null (ExpiringSet _ hashMap) = A.null hashMap
{-# INLINE null #-}

size :: ExpiringSet a -> Int
size (ExpiringSet _ hashMap) = A.size hashMap

member :: (Eq a, Hashable a) => a -> ExpiringSet a -> Bool
member a (ExpiringSet _ hashMap) = A.member a hashMap

memberTime :: UTCTime -> ExpiringSet a -> Bool
memberTime time (ExpiringSet intMultiMap _) = B.member key intMultiMap
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time

insertForce :: (Hashable element, Eq element) => UTCTime {-^ Expiry time -} -> element -> ExpiringSet element -> ExpiringSet element
insertForce time value (ExpiringSet intMultiMap hashMap) =
  ExpiringSet newMultiMap (A.insert value key hashMap)
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time
    maybeTimestamp = A.lookup value hashMap
    newMultiMap = case maybeTimestamp of
      Nothing -> B.insert key value intMultiMap
      Just k -> B.insert key value $ B.delete k value intMultiMap

{-|

-}
insert :: (Hashable element, Eq element) => UTCTime {-^ Expiry time -} -> element -> ExpiringSet element -> ExpiringSet element
insert time value (ExpiringSet intMultiMap hashMap) =
  ExpiringSet newMultiMap newHash
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time
    maybeTimestamp = A.lookup value hashMap
    (newMultiMap, newHash) = case maybeTimestamp of
      Nothing -> (B.insert key value intMultiMap, A.insert value key hashMap)
      Just k -> if key >= k
        then (B.insert key value $ B.delete k value intMultiMap, A.insert value key hashMap)
        else (intMultiMap, hashMap)

deleteByTime :: (Hashable element, Eq element) => UTCTime {-^ Expiry time -} -> element -> ExpiringSet element -> ExpiringSet element
deleteByTime time element (ExpiringSet _ hashMap) =
  construct $ A.delete element hashMap
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time

delete :: (Hashable element, Eq element) => element -> ExpiringSet element -> ExpiringSet element
delete value (ExpiringSet intMultiMap hashMap) =
  ExpiringSet newMultiMap (A.delete value hashMap)
  where
    maybeKey = A.lookup value hashMap
    newMultiMap = case maybeKey of
      Nothing -> intMultiMap
      Just key -> B.delete key value intMultiMap
