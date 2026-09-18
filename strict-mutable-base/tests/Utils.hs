module Utils
  ( bomb
  , assertForced
  , assertNotForced
  , assertNotForcedWhenBuilt
  ) where

import Control.Exception
import Test.Tasty.HUnit
import qualified GHC.Exts as E

-- | A value that throws when forced to WHNF.
bomb :: a
bomb = error bombMessage

bombMessage :: String
bombMessage = "bomb"

-- | Assert that the action forces the 'bomb' it was handed.
assertForced :: String -> IO a -> Assertion
assertForced preface action = try @ErrorCall action >>= \case
  Left (ErrorCall msg) -> assertEqual (preface ++ ": unexpected error") bombMessage msg
  Right _ -> assertFailure $ preface ++ ": the value was not forced"

-- | Assert that the action doesn't force the value it was handed past WHNF.
assertNotForced :: String -> IO a -> Assertion
assertNotForced preface action = try @ErrorCall action >>= \case
  Left err -> assertFailure $ preface ++ ": the value was forced (" ++ show err ++ ")"
  Right _ -> pure ()

-- | Assert that the action doesn't force the value it was handed until it runs.
--
-- The action is only evaluated to WHNF, never run.
assertNotForcedWhenBuilt :: String -> IO a -> Assertion
assertNotForcedWhenBuilt preface action = do
  -- E.lazy stops the caller from evaluating the action outside of try.
  r <- try @ErrorCall . evaluate $ E.lazy action `seq` ()
  case r of
    Left err -> assertFailure $ preface ++ ": the value was forced (" ++ show err ++ ")"
    Right () -> pure ()
