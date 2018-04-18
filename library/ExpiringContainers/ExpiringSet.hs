module ExpiringContainers.ExpiringSet
  where

import Data.HashMap.Strict as A
import ExpiringContainers.IntMultiMap as B
import Data.HashSet as C
import Data.Time
import Data.Int
import Prelude
import Timestamp
import Data.Hashable
import Data.Foldable

{-|

-}
data ExpiringSet element =
  ExpiringSet
    {-| Elements indexed by timestamps -}
    (IntMultiMap element)
    {-| Timestamps indexed by elements -}
    (HashMap element Int)


clean :: (Hashable element, Ord element) => UTCTime -> ExpiringSet element -> ([element], ExpiringSet element)
clean time (ExpiringSet intMultiMap hashMap) =
  (listElem, ExpiringSet newMultiMap newHash)
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time
    newHash = A.filterWithKey (\_ k -> k >= key) hashMap
    (oldMultiMap, newMultiMap) = (B.splitExpiring key intMultiMap)
    listElem = Data.Foldable.toList oldMultiMap

insert :: (Hashable element, Ord element) => UTCTime {-^ Expiry time -} -> element -> ExpiringSet element -> ExpiringSet element
insert time value (ExpiringSet intMultiMap hashMap) =
  ExpiringSet newMultiMap (A.insert value key hashMap)
  where
    key = fromIntegral $ (timestampMicroSecondsInt64 . utcTimeTimestamp) time
    startKey = A.lookup value hashMap
    newMultiMap = B.insert key value $ case startKey of
      Just k -> B.delete k value intMultiMap
      Nothing -> intMultiMap
