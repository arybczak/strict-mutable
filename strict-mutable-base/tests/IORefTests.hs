module IORefTests (ioRefTests) where

import Data.IORef.Strict
import System.Mem.Weak
import Test.Tasty
import Test.Tasty.HUnit

import Utils

ioRefTests :: TestTree
ioRefTests = testGroup "IORef"
  [ testCase "basic operations" test_basicOperations
  , testCase "mkWeakIORef" test_mkWeakIORef
  , testCase "values are forced" test_valuesAreForced
  , testCase "values are forced only to WHNF" test_valuesAreForcedOnlyToWHNF
  ]

test_basicOperations :: Assertion
test_basicOperations = do
  ref <- newIORef (1 :: Int)
  readIORef ref >>= assertEqual "initial value" 1
  writeIORef ref 2
  readIORef ref >>= assertEqual "value after writeIORef" 2
  modifyIORef ref (+ 1)
  readIORef ref >>= assertEqual "value after modifyIORef" 3
  atomicWriteIORef ref 4
  readIORef ref >>= assertEqual "value after atomicWriteIORef" 4
  out <- atomicModifyIORef ref $ \a -> (a * 2, show a)
  assertEqual "result of atomicModifyIORef" "4" out
  readIORef ref >>= assertEqual "value after atomicModifyIORef" 8

test_mkWeakIORef :: Assertion
test_mkWeakIORef = do
  ref <- newIORef (1 :: Int)
  weak <- mkWeakIORef ref $ pure ()
  deRefWeak weak >>= \case
    Just ref' -> assertBool "weak pointer points at the ref" (ref' == ref)
    Nothing -> assertFailure "weak pointer is dead"

test_valuesAreForced :: Assertion
test_valuesAreForced = do
  assertForced "newIORef" $ newIORef bomb
  ref <- newIORef (1 :: Int)
  assertForced "writeIORef" $ writeIORef ref bomb
  assertForced "modifyIORef" $ modifyIORef ref (const bomb)
  assertForced "atomicWriteIORef" $ atomicWriteIORef ref bomb
  assertForced "atomicModifyIORef (result)" $ atomicModifyIORef ref $ \a -> (a, bomb)
  readIORef ref >>= assertEqual "value is intact" 1
  -- A failed atomicModifyIORef installs the bottoming value, so it goes last.
  assertForced "atomicModifyIORef (new value)" $ atomicModifyIORef ref $ \_ -> (bomb, ())

test_valuesAreForcedOnlyToWHNF :: Assertion
test_valuesAreForcedOnlyToWHNF = do
  ref <- newIORef (Just (bomb :: Int))
  assertNotForced "writeIORef" $ writeIORef ref (Just bomb)
  assertNotForced "modifyIORef" $ modifyIORef ref (const (Just bomb))
  assertNotForced "atomicWriteIORef" $ atomicWriteIORef ref (Just bomb)
  assertNotForced "atomicModifyIORef" $ atomicModifyIORef ref $ \a -> (a, Just bomb)
