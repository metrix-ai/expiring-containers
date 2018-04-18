module ExpiringContainers.IntMultiMap
  where

import qualified Data.IntMap.Strict as A
import qualified Data.HashSet as B
import Prelude
import Data.Int
import Data.Hashable
import Data.Foldable

{-|

-}
newtype IntMultiMap value =
  IntMultiMap (A.IntMap (B.HashSet value))
  deriving(Foldable)

delete :: (Hashable value, Eq value) => Int {-^ Key -} -> value -> IntMultiMap value -> IntMultiMap value
delete key value (IntMultiMap intMap) =
  IntMultiMap $ A.update f key intMap
  where
    f hashSet =
      mfilter (not . B.null) . Just $ B.delete value hashSet

insert :: (Hashable value, Ord value) => Int -> value -> IntMultiMap value -> IntMultiMap value
insert key value (IntMultiMap intMap) =
  IntMultiMap $ A.update (\hash -> Just $ B.insert value hash) key intMap

deleteExpring :: Int -> IntMultiMap value -> IntMultiMap value
deleteExpring key (IntMultiMap intMap) = IntMultiMap $ A.filterWithKey (\k _ -> k >= key) intMap

splitExpiring :: Int -> IntMultiMap value -> (IntMultiMap value, IntMultiMap value)
splitExpiring key (IntMultiMap intMap) = (IntMultiMap oldMap, IntMultiMap newMap)
  where
    (oldMap, newMap) = A.split key intMap
