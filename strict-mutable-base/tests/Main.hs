module Main (main) where

import Test.Tasty

import ChanTests
import IORefTests
import MVarTests

main :: IO ()
main = defaultMain $ testGroup "strict-mutable-base"
  [ ioRefTests
  , mVarTests
  , chanTests
  ]
