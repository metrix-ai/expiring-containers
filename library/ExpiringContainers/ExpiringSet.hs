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

{-|
Set that expiring with time
-}
data ExpiringSet element =
  ExpiringSet
    {-| Elements indexed by timestamps -}
    (B.IntMultiMap element)
    {-| Timestamps indexed by elements -}
    (A.HashMap element Int)
    deriving(Eq, Show, Generic)



{--------------------------------------------------------------------
  Transformations
--------------------------------------------------------------------}
map :: (Eq b, Hashable b) => (a -> b) -> ExpiringSet a -> ExpiringSet b
map f (ExpiringSet intMultiMap _) = construct $ B.map f intMultiMap
{-# INLINE map #-}

construct :: (Eq a, Hashable a) => B.IntMultiMap a -> ExpiringSet a
construct intMultiMap = ExpiringSet intMultiMap $
  A.fromList $ fmap (\(k, v) -> (v, k)) (B.toList intMultiMap)

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
fromList = construct . B.fromList . fmap (\(t, a) -> (,) (fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) t) a)

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
empty :: (Eq a, Hashable a) => ExpiringSet a
empty = ExpiringSet B.empty A.empty

singleton :: (Eq a, Hashable a) => UTCTime -> a -> ExpiringSet a
singleton time v = construct $ B.singleton key v
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time
{-# INLINABLE singleton #-}


{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
{-|
Clean expiringset
-}
clean :: (Hashable element, Ord element) => UTCTime -> ExpiringSet element -> ([element], ExpiringSet element)
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
{-|

-}
insert :: (Hashable element, Ord element) => UTCTime {-^ Expiry time -} -> element -> ExpiringSet element -> ExpiringSet element
insert time value (ExpiringSet intMultiMap hashMap) =
  ExpiringSet newMultiMap (A.insert value key hashMap)
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time
    newMultiMap = B.insert key value intMultiMap
