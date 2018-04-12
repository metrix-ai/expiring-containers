module ExpiringContainers.ExpiringMap
  where

import ExpiringContainers.ExpiringSet
import Data.HashMap.Strict

data ExpiringMap key value =
  ExpiringMap
    (ExpiringSet key)
    (HashMap key value)



insert :: UTCTime {-^ Expiry time -} -> key -> value -> ExpiringMap key value -> ExpiringMap key value
insert =
  $(todo "")

lookup :: key -> ExpiringMap key value -> Maybe value
lookup =
  $(todo "")

setCurrentTime :: UTCTime -> ExpiringMap key value -> ExpiringMap key value
setCurrentTime =
  $(todo "")
