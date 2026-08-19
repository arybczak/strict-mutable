module ChanTests (chanTests) where

import Control.Concurrent.Chan.Strict
import Test.Tasty
import Test.Tasty.HUnit

import Utils

chanTests :: TestTree
chanTests = testGroup "Chan"
  [ testCase "basic operations" test_basicOperations
  , testCase "values are forced" test_valuesAreForced
  , testCase "values are forced only to WHNF" test_valuesAreForcedOnlyToWHNF
  ]

test_basicOperations :: Assertion
test_basicOperations = do
  chan <- newChan @Int
  writeChan chan 1
  writeList2Chan chan [2, 3]
  readChan chan >>= assertEqual "first value" 1
  dup <- dupChan chan
  writeChan chan 4
  readChan dup >>= assertEqual "value seen by the duplicate" 4
  contents <- getChanContents chan
  assertEqual "contents of the original" [2, 3, 4] (take 3 contents)

test_valuesAreForced :: Assertion
test_valuesAreForced = do
  chan <- newChan @Int
  assertForced "writeChan" $ writeChan chan bomb
  assertForced "writeList2Chan" $ writeList2Chan chan [1, bomb]
  readChan chan >>= assertEqual "only the first value was written" 1

test_valuesAreForcedOnlyToWHNF :: Assertion
test_valuesAreForcedOnlyToWHNF = do
  chan <- newChan
  assertNotForced "writeChan" $ writeChan chan (Just (bomb :: Int))
  assertNotForced "writeList2Chan" $ writeList2Chan chan [Just bomb]
