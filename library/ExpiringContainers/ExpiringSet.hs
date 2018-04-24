module ExpiringContainers.ExpiringSet
(
  ExpiringSet,
  -- * Construction

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

  -- * Conversions
  empty,
  singleton,

  -- * Filter
  clean,
)
where

import qualified Data.HashMap.Strict as A
import qualified IntMultiMap as B
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
  type Item (ExpiringSet a) = (Int, a)
  toList = toList
  fromList = fromList

toList :: ExpiringSet b -> [(Int, b)]
toList (ExpiringSet intMultiMap _) = B.toList intMultiMap

fromList :: (Eq a, Hashable a) =>
     [(Int, a)] -> ExpiringSet a
fromList = construct . B.fromList

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
empty :: (Eq a, Hashable a) => ExpiringSet a
empty = ExpiringSet B.empty A.empty

singleton :: (Eq a, Hashable a) => Int -> a -> ExpiringSet a
singleton k v = construct $ B.singleton k v
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
    (oldMultiMap, newMultiMap) = (B.split key intMultiMap)
    listElem = D.toList oldMultiMap

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

memberTime :: Int -> ExpiringSet a -> Bool
memberTime key (ExpiringSet intMultiMap _) = B.member key intMultiMap

{-|

-}
insert :: (Hashable element, Ord element) => UTCTime {-^ Expiry time -} -> element -> ExpiringSet element -> ExpiringSet element
insert time value (ExpiringSet intMultiMap hashMap) =
  ExpiringSet newMultiMap (A.insert value key hashMap)
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time
    startKey = A.lookup value hashMap
    newMultiMap = B.insert key value $ case startKey of
      Just k -> B.delete k value intMultiMap
      Nothing -> intMultiMap
