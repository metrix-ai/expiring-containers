module Main where

import Prelude hiding (first, second)
import Control.Arrow
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
-- import Test.QuickCheck.Instances
import Test.QuickCheck.Instances.Time
import qualified ExpiringContainers.ExpiringSet as A
import qualified ExpiringContainers.ExpiringMap as B
import qualified Data.List as C
import Data.Maybe
import Data.Time
import Data.Foldable as F
import qualified Data.Set as D
import qualified Data.HashMap.Strict as E
import Timestamp

main =
  defaultMain $
  testGroup "All tests" $
  [
    expiringSet
    ,
    expiringMap
  ]

expiringSet =
  testGroup "Expiring Set" $
  [
    testProperty "list to list" $ \ (list :: [(UTCTime, Int)]) ->
    (D.fromList $ C.nubBy (\(t1,v1) (t2,v2) -> v1 == v2) $ C.reverse $ map (\(t,a) -> (,) (timestampUtcTime $ utcTimeTimestamp t) a) list) ===
       (D.fromList $ A.toList $ A.fromList list)
    ,
    testProperty "delete" $ \ (list :: [(UTCTime, Int)], key :: UTCTime, value :: Int) ->
    let list2 = C.nubBy (\(t1,k1) (t2,k2) -> k1 == k2) list
    in (D.fromList $ fmap (\(t,a) -> (,) (timestampUtcTime $ utcTimeTimestamp t) a) $
     C.deleteBy (\(_,v1) (_,v2) -> v1 == v2) (key, value) list2) ===
        (D.fromList $ A.toList $ A.delete value $ A.fromList list2)
    ,
    testProperty "insert and insert" $ \ (list :: [(UTCTime, Int)], key1 :: UTCTime, value1 :: Int, key2 :: UTCTime,  value2 :: Int) ->
    let set = A.fromList list
    in if key1 == key2 || value1 == value2
        then (A.insert key1 value1 $ A.insert key1 value2 set) === (A.insert key1 value1 set)
        else (A.insert key1 value1 $ A.insert key2 value2 set) === (A.insert key2 value2 $ A.insert key1 value1 set)
    ,
    testProperty "delete ans insert" $ \ (list :: [(UTCTime, Int)], key :: UTCTime, value1 :: Int, value2 :: Int) ->
    let set = A.fromList list
    in if value1 == value2
        then (A.delete value1 $ A.insert key value1 set) === (A.delete value1 set)
        else (A.delete value1 $ A.insert key value2 set) === (A.insert key value2 $ A.delete value1 set)
    ,
    testProperty "member and insert" $ \ (list :: [(UTCTime, Int)], key :: UTCTime, value1 :: Int, value2 :: Int) ->
    let set = A.fromList list
    in if value1 == value2
        then (A.member value1 $ A.insert key value1 set) === True
        else (A.member value1 $ A.insert key value2 set) === (A.member value1 set)
    ,
    testProperty "member and delete" $ \ (list :: [(UTCTime, Int)], value1 :: Int, value2 :: Int) ->
    let set = A.fromList list
    in if value1 == value2
        then (A.member value1 $ A.delete value1 set) === False
        else (A.member value1 $ A.delete value2 set) === (A.member value1 set)
    ,
    testProperty "map" $ \ (list :: [(UTCTime, Int)], value1 :: Int, value2 :: Int) ->
    let set = A.fromList list
    in A.map id set === id set
    ,
    testProperty "map insert" $ \ (list :: [(UTCTime, Int)], key :: UTCTime, value1 :: Int, fun :: (Fun Int Char)) ->
    let set = A.fromList list
        f = applyFun fun
    in A.map f (A.insert key value1 set) === A.insert key (f value1) (A.map f set)
    ,
    -- testProperty "map delete" $ \ (list :: [(UTCTime, Int)], key :: UTCTime, value1 :: Int, fun :: (Fun Int Char)) ->
    -- let set = A.fromList list
    --     f = applyFun fun
    -- in A.map f (A.delete value1 set) === A.delete (f value1) (A.map f set)
    -- ,
    -- testProperty "clean" $ \ (list :: [(UTCTime, Int)], key :: UTCTime) ->
    -- let (list1, expire) = A.clean key $ A.fromList list
    -- in (D.fromList $ fmap (\(t,a) -> a) $ C.filter (\(t,a) -> t < key) list) === (D.fromList list1)
    -- ,
    -- testProperty "mapping" $ \ (list :: [(UTCTime, Int)]) ->
    -- (D.fromList $ map (testFuncL . (\(t,a) -> (,) (timestampUtcTime $ utcTimeTimestamp t) a)) list) ===
    --    (D.fromList $ A.toList $ A.map testFunc $ A.fromList list)
    -- ,
    testProperty "member" $ \ (list :: [(UTCTime, Int)], key :: UTCTime, value :: Int) ->
    (A.member value $ A.delete value $ A.insert key value $ A.fromList list) === False
    ,
    testProperty "lookup not found" $ \ (list :: [(UTCTime, Int)], key :: UTCTime, value :: Int) ->
        (A.lookup value $ A.delete value $ A.insert key value $ A.fromList list) === Nothing
    ,
    testProperty "lookup found" $ \ (list :: [(UTCTime, Int)], key :: UTCTime, value :: Int) ->
        (A.lookup value $ A.insertForce key value $ A.fromList list) === (Just $ timestampUtcTime $ utcTimeTimestamp key)
    ,
    testProperty "foldr" $ \ (list :: [(UTCTime, Int)], seed :: Int) ->
        (foldr (\a b -> a + b) seed $ A.fromList list) === (foldr (\(t, a) b -> a + b) seed $ A.toList $ A.fromList list)
    ,
    testProperty "foldMap" $ \ (list :: [(UTCTime, Int)]) ->
        (foldMap (show) $ A.fromList list) === (foldMap (\(t, a) -> show a) $ A.toList $ A.fromList list)
  ]

expiringMap =
  testGroup "Expiring Map" $
  [
    testProperty "list to list" $ \ (list :: [(UTCTime, Int, Int)]) ->
    let list2 = C.nubBy (\(t1,k1,v1) (t2,k2,v2) -> k1 == k2) list
    in (D.fromList $ fmap (\(t,a,b) -> (,,) (timestampUtcTime $ utcTimeTimestamp t) a b) list2) ===
       (D.fromList $ B.toList $ B.fromList list2)
    ,
    testProperty "delete" $ \ (list :: [(UTCTime, Int, Int)], time :: UTCTime, key :: Int, value :: Int) ->
    let list2 = C.nubBy (\(t1,k1,v1) (t2,k2,v2) -> k1 == k2) list
    in (D.fromList $
      fmap (\(t,a,b) -> (,,) (timestampUtcTime $ utcTimeTimestamp t) a b) $
      C.deleteBy (\(t1,k1,v1) (t2,k2,v2) -> k1 == k2) (time, key, value) list2) ===
         (D.fromList $ B.toList $ B.delete key $ B.fromList list2)
    ,
    testProperty "insert and insert" $ \ (list :: [(UTCTime, Int, Int)], time1 :: UTCTime, key1 :: Int, value1 :: Int, time2 :: UTCTime, key2 :: Int, value2 :: Int) ->
    let map' = B.fromList list
    in if time1 == time2 || key1 == key2
        then (B.insert time1 key1 value1 $ B.insert time1 key1 value2 map') === (B.insert time1 key1 value1 map')
        else (B.insert time1 key1 value1 $ B.insert time2 key2 value2 map') === (B.insert time2 key2 value2 $ B.insert time1 key1 value1 map')
    ,
    testProperty "delete and insert" $ \ (list :: [(UTCTime, Int, Int)], time :: UTCTime, key1 :: Int, key2 :: Int, value :: Int) ->
    let map' = B.fromList list
    in if key1 == key2
        then (B.delete key1 $ B.insert time key1 value map') === (B.delete key1 map')
        else (B.delete key1 $ B.insert time key2 value map') === (B.insert time key2 value $ B.delete key1 map')
    ,
    testProperty "member and insert" $ \ (list :: [(UTCTime, Int, Int)], time :: UTCTime, key1 :: Int, key2 :: Int, value :: Int) ->
    let map' = B.fromList list
    in if key1 == key2
        then (B.member key1 $ B.insert time key1 value map') === True
        else (B.member key1 $ B.insert time key2 value map') === (B.member key1 map')
    ,
    testProperty "member and delete" $ \ (list :: [(UTCTime, Int, Int)], key1 :: Int, key2 :: Int) ->
    let map' = B.fromList list
    in if key1 == key2
        then (B.member key1 $ B.delete key1 map') === False
        else (B.member key1 $ B.delete key2 map') === (B.member key1 map')
    ,
    testProperty "member and insert" $ \ (list :: [(UTCTime, Int, Int)], time :: UTCTime, key1 :: Int, key2 :: Int, value :: Int) ->
    let map' = B.fromList list
    in if key1 == key2
        then (B.lookup key1 $ B.insert time key1 value map') === Just value
        else (B.lookup key1 $ B.insert time key2 value map') === (B.lookup key1 map')
    ,
    testProperty "lookup and delete" $ \ (list :: [(UTCTime, Int, Int)], key1 :: Int, key2 :: Int) ->
    let map' = B.fromList list
    in if key1 == key2
        then (B.lookup key1 $ B.delete key1 map') === Nothing
        else (B.lookup key1 $ B.delete key2 map') === (B.lookup key1 map')
    ,
    testProperty "map" $ \ (list :: [(UTCTime, Int, Int)], value1 :: Int, value2 :: Int) ->
    let map' = B.fromList list
    in B.map id map' === id map'
    ,
    testProperty "map insert" $ \ (list :: [(UTCTime, Int, Int)], time :: UTCTime, key :: Int, value1 :: Int, fun :: (Fun Int Char)) ->
    let map' = B.fromList list
        f = applyFun fun
    in B.map f (B.insert time key value1 map') === B.insert time key (f value1) (B.map f map')
    ,
    testProperty "map delete" $ \ (list :: [(UTCTime, Int, Int)], time :: UTCTime, key :: Int, value1 :: Int, fun :: (Fun Int Char)) ->
    let map' = B.fromList list
        f = applyFun fun
    in B.map f (B.delete key map') === B.delete key (B.map f map')
    ,
    testProperty "map lookup" $ \ (list :: [(UTCTime, Int, Int)], time :: UTCTime, key :: Int, value1 :: Int, fun :: (Fun Int Char)) ->
    let map' = B.fromList list
        f = applyFun fun
    in fmap f (B.lookup key map') === B.lookup key (B.map f map')
    ,
    -- testProperty "map" $ \ (list :: [(UTCTime, Int, Int)], time :: UTCTime, key :: Int, value :: Int) ->
    -- let map' = B.fromList list
    -- in fmap (\x -> B.insert time key value map') (B.lookup key map') === (map' <$ (B.lookup key map'))
    -- ,
    -- testProperty "clean" $ \ (list :: [(UTCTime, Int)], key :: UTCTime) ->
    -- let (list1, expire) = A.clean key $ A.fromList list
    -- in (D.fromList $ fmap (\(t,a) -> a) $ C.filter (\(t,a) -> t < key) list) === (D.fromList list1)
    -- ,
    testProperty "member" $ \ (list :: [(UTCTime, Int, Int)], time :: UTCTime, key :: Int, value :: Int) ->
    (B.member key $ B.delete key $ B.insert time key value $ B.fromList list) === False
    -- ,
    -- testProperty "insert and delete" $ \ (list :: [(UTCTime, Int, Int)], time :: UTCTime, key :: Int, value :: Int) ->
    -- let list2 = C.nubBy (\(t1,k1,v1) (t2,k2,v2) -> k1 == k2) list
    -- in (D.fromList $ fmap (\(t,a,b) -> (,,) (timestampUtcTime $ utcTimeTimestamp t) a b) $
    --  C.deleteBy (\(t1,k1,v1) (t2,k2,v2) -> k1 == k2) (time, key, value) list2) === (D.fromList $ B.toList $ B.delete key $ B.insert time key value $ B.fromList list)

  ]

testFunc :: (Show a) => a -> String
testFunc = show

testFuncL :: (Show a)  => (UTCTime, a) -> (UTCTime, String)
testFuncL (a, b) = (a, show b)
