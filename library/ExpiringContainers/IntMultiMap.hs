module ExpiringContainers
  where

import Data.IntMap.Strict
import Data.HashMap.Strict

newtype IntMultiMap value =
  IntMultiMap (IntMap (HashSet value))

delete :: Int {-^ Key -} -> IntMultiMap value -> IntMultiMap value
delete =
  $(todo "Delete the value from the hashset, and if the hashset becomes empty afterwards, \
    \delete the whole association by key")
