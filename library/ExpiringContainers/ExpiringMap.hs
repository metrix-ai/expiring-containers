module ExpiringContainers.ExpiringMap
(
  ExpiringMap,
  insert,
  lookup,
  setCurrentTime,
)
where

import qualified ExpiringContainers.ExpiringSet as A
import qualified Data.HashMap.Strict as B
import Data.Time
import Data.Maybe
import Prelude hiding (lookup)
import Data.Hashable
import qualified Data.List as C

{-|

-}
data ExpiringMap key value =
  ExpiringMap
    (A.ExpiringSet key)
    (B.HashMap key value)

insert :: (Eq key, Ord key, Hashable key) => UTCTime {-^ Expiry time -} -> key -> value -> ExpiringMap key value -> ExpiringMap key value
insert time key value (ExpiringMap expSet hashMap) =
  ExpiringMap (A.insert time key expSet) (B.insert key value hashMap)

lookup :: (Eq key, Hashable key) => key -> ExpiringMap key value -> Maybe value
lookup key (ExpiringMap expSet hashMap) =
  B.lookup key hashMap

setCurrentTime :: (Eq key, Ord key, Hashable key) => UTCTime -> ExpiringMap key value -> ExpiringMap key value
setCurrentTime time (ExpiringMap expSet hashMap) =
  ExpiringMap newExpSet newHashMap
    where
      (keys, newExpSet) = A.clean time expSet
      newHashMap = C.foldl' (flip B.delete) hashMap keys
