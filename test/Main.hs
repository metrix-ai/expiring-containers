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
    (D.fromList $ fmap (\(t,a) -> (,) (timestampUtcTime $ utcTimeTimestamp t) a) list) === (D.fromList $ A.toList $ A.fromList list)
    ,
    testProperty "delete" $ \ (list :: [(UTCTime, Int)], key :: UTCTime, value :: Int) ->
    (D.fromList $ fmap (\(t,a) -> (,) (timestampUtcTime $ utcTimeTimestamp t) a) $ C.delete (key, value) list) === (D.fromList $ A.toList $ A.delete key value $ A.fromList list)
    ,
    testProperty "insert" $ \ (list :: [(UTCTime, Int)], key :: UTCTime, value :: Int) ->
    (D.fromList $ fmap (\(t,a) -> (,) (timestampUtcTime $ utcTimeTimestamp t) a) $ C.insert (key, value) list) === (D.fromList $ A.toList $ A.insert key value $ A.fromList list)
    ,
    testProperty "insert and delete" $ \ (list :: [(UTCTime, Int)], key :: UTCTime, value :: Int) ->
    (D.fromList $ fmap (\(t,a) -> (,) (timestampUtcTime $ utcTimeTimestamp t) a) list) === (D.fromList $ A.toList $ A.delete key value $ A.insert key value $ A.fromList list)
    ,
    testProperty "clean" $ \ (list :: [(UTCTime, Int)], key :: UTCTime) ->
    let (list1, expire) = A.clean key $ A.fromList list
    in (D.fromList $ fmap (\(t,a) -> a) $ C.filter (\(t,a) -> t < key) list) === (D.fromList list1)
    ,
    testProperty "mapping" $ \ (list :: [(UTCTime, Int)]) ->
    (D.fromList $ map (testFuncL . (\(t,a) -> (,) (timestampUtcTime $ utcTimeTimestamp t) a)) list) === (D.fromList $ A.toList $ A.map testFunc $ A.fromList list)
  ]

expiringMap =
  testGroup "Expiring Map" $
  [
  ]

testFunc :: (Show a) => a -> String
testFunc = show

testFuncL :: (Show a)  => (UTCTime, a) -> (UTCTime, String)
testFuncL (a, b) = (a, show b)
