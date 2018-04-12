module ExpiringSet
  where

import ExpiringContainers.IntMultiMap
import Data.HashMap.Strict

{-|

-}
data ExpiringSet element =
  ExpiringSet
    {-| Elements indexed by timestamps -}
    (IntMultiMap element)
    {-| Timestamps indexed by elements -}
    (HashMap element Int)


setCurrentTime :: UTCTime -> ExpiringSet element -> ExpiringSet element
setCurrentTime =
  $(todo "")

insert :: UTCTime {-^ Expiry time -} -> element -> ExpiringSet element -> ExpiringSet element
insert =
  $(todo "")
