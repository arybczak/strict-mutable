module MVarTests (mVarTests) where

import Control.Concurrent.MVar.Strict
import System.Mem.Weak
import Test.Tasty
import Test.Tasty.HUnit

import Utils

mVarTests :: TestTree
mVarTests = testGroup "MVar"
  [ testCase "basic operations" test_basicOperations
  , testCase "mkWeakMVar" test_mkWeakMVar
  , testCase "values are forced" test_valuesAreForced
  , testCase "values are forced only to WHNF" test_valuesAreForcedOnlyToWHNF
  ]

test_basicOperations :: Assertion
test_basicOperations = do
  var <- newEmptyMVar @Int
  isEmptyMVar var >>= assertBool "new var is empty"
  tryTakeMVar var >>= assertEqual "tryTakeMVar on an empty var" Nothing
  tryReadMVar var >>= assertEqual "tryReadMVar on an empty var" Nothing
  tryPutMVar var 1 >>= assertBool "tryPutMVar on an empty var"
  isEmptyMVar var >>= assertBool "var is full" . not
  tryPutMVar var 2 >>= assertBool "tryPutMVar on a full var" . not
  tryReadMVar var >>= assertEqual "tryReadMVar on a full var" (Just 1)
  readMVar var >>= assertEqual "value from readMVar" 1
  takeMVar var >>= assertEqual "value from takeMVar" 1
  putMVar var 3
  swapMVar var 4 >>= assertEqual "old value from swapMVar" 3
  withMVar var $ assertEqual "value in withMVar" 4
  withMVarMasked var $ assertEqual "value in withMVarMasked" 4
  modifyMVar_ var $ pure . (+ 1)
  readMVar var >>= assertEqual "value after modifyMVar_" 5
  modifyMVarMasked_ var $ pure . (+ 1)
  readMVar var >>= assertEqual "value after modifyMVarMasked_" 6
  out <- modifyMVar var $ \a -> pure (a + 1, show a)
  assertEqual "result of modifyMVar" "6" out
  outMasked <- modifyMVarMasked var $ \a -> pure (a + 1, show a)
  assertEqual "result of modifyMVarMasked" "7" outMasked
  readMVar var >>= assertEqual "final value" 8

test_mkWeakMVar :: Assertion
test_mkWeakMVar = do
  var <- newMVar (1 :: Int)
  weak <- mkWeakMVar var $ pure ()
  deRefWeak weak >>= \case
    Just var' -> assertBool "weak pointer points at the var" (var' == var)
    Nothing -> assertFailure "weak pointer is dead"

test_valuesAreForced :: Assertion
test_valuesAreForced = do
  assertForced "newMVar" $ newMVar bomb
  emptyVar <- newEmptyMVar @Int
  assertForced "putMVar" $ putMVar emptyVar bomb
  assertForced "tryPutMVar" $ tryPutMVar emptyVar bomb
  isEmptyMVar emptyVar >>= assertBool "var is still empty"
  var <- newMVar (1 :: Int)
  assertForced "swapMVar" $ swapMVar var bomb
  assertForced "modifyMVar_" $ modifyMVar_ var $ \_ -> pure bomb
  assertForced "modifyMVar" $ modifyMVar var $ \_ -> pure (bomb, ())
  assertForced "modifyMVarMasked_" $ modifyMVarMasked_ var $ \_ -> pure bomb
  assertForced "modifyMVarMasked" $ modifyMVarMasked var $ \_ -> pure (bomb, ())
  readMVar var >>= assertEqual "value is intact" 1

test_valuesAreForcedOnlyToWHNF :: Assertion
test_valuesAreForcedOnlyToWHNF = do
  emptyVar <- newEmptyMVar
  assertNotForced "putMVar" $ putMVar emptyVar (Just (bomb :: Int))
  var <- newMVar (Just (bomb :: Int))
  assertNotForced "swapMVar" $ swapMVar var (Just bomb)
  assertNotForced "modifyMVar_" $ modifyMVar_ var $ \_ -> pure (Just bomb)
  assertNotForced "modifyMVar (result)" $ modifyMVar var $ \a -> pure (a, bomb)
  assertNotForced "modifyMVarMasked (result)" $ modifyMVarMasked var $ \a -> pure (a, bomb)
