-- | A strict variant of t'Control.Concurrent.MVar.MVar' that keeps its
-- contents in weak head normal form (WHNF).
--
-- For full documentation please refer to "Control.Concurrent.MVar".
module Control.Concurrent.MVar.Strict
  ( MVar

    -- * Operations
  , newEmptyMVar
  , newMVar
  , takeMVar
  , putMVar
  , readMVar
  , swapMVar
  , tryTakeMVar
  , tryPutMVar
  , tryReadMVar
  , isEmptyMVar
  , withMVar
  , withMVarMasked
  , modifyMVar_
  , modifyMVar
  , modifyMVarMasked_
  , modifyMVarMasked
  , mkWeakMVar
  ) where

import Control.DeepSeq
import GHC.Exts (mkWeak#)
import GHC.IO (IO(..))
import GHC.Weak (Weak(..))
import qualified Control.Concurrent.MVar as Lazy
import qualified GHC.MVar as GHC

-- | A strict (WHNF) variant of t'Control.Concurrent.MVar.MVar'.
newtype MVar a = MVar (GHC.MVar a)
  deriving (Eq, NFData, NFData1)

-- | 'Control.Concurrent.MVar.newEmptyMVar' for a strict t'MVar'.
newEmptyMVar :: IO (MVar a)
newEmptyMVar = MVar <$> Lazy.newEmptyMVar

-- | 'Control.Concurrent.MVar.newMVar' for a strict t'MVar'.
--
-- Evaluates the initial value to WHNF.
newMVar :: a -> IO (MVar a)
newMVar a = fmap MVar . Lazy.newMVar =<< (pure $! a)

-- | 'Control.Concurrent.MVar.takeMVar' for a strict t'MVar'.
takeMVar :: MVar a -> IO a
takeMVar (MVar var) = Lazy.takeMVar var

-- | 'Control.Concurrent.MVar.putMVar' for a strict t'MVar'.
--
-- Evaluates the new value to WHNF.
putMVar :: MVar a -> a -> IO ()
putMVar (MVar var) a = Lazy.putMVar var =<< (pure $! a)

-- | 'Control.Concurrent.MVar.readMVar' for a strict t'MVar'.
readMVar :: MVar a -> IO a
readMVar (MVar var) = Lazy.readMVar var

-- | 'Control.Concurrent.MVar.swapMVar' for a strict t'MVar'.
--
-- Evaluates the new value to WHNF.
swapMVar :: MVar a -> a -> IO a
swapMVar (MVar var) a = Lazy.swapMVar var =<< (pure $! a)

-- | 'Control.Concurrent.MVar.tryTakeMVar' for a strict t'MVar'.
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar var) = Lazy.tryTakeMVar var

-- | 'Control.Concurrent.MVar.tryPutMVar' for a strict t'MVar'.
--
-- Evaluates the new value to WHNF.
tryPutMVar :: MVar a -> a -> IO Bool
tryPutMVar (MVar var) a = Lazy.tryPutMVar var =<< (pure $! a)

-- | 'Control.Concurrent.MVar.tryReadMVar' for a strict t'MVar'.
tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar (MVar var) = Lazy.tryReadMVar var

-- | 'Control.Concurrent.MVar.isEmptyMVar' for a strict t'MVar'.
isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar (MVar var) = Lazy.isEmptyMVar var

-- | 'Control.Concurrent.MVar.withMVar' for a strict t'MVar'.
withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar (MVar var) action = Lazy.withMVar var action
{-# INLINE withMVar #-}

-- | 'Control.Concurrent.MVar.withMVarMasked' for a strict t'MVar'.
withMVarMasked :: MVar a -> (a -> IO b) -> IO b
withMVarMasked (MVar var) action = Lazy.withMVarMasked var action
{-# INLINE withMVarMasked #-}

-- | 'Control.Concurrent.MVar.modifyMVar_' for a strict t'MVar'.
--
-- Evaluates the new value to WHNF.
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ (MVar var) action = Lazy.modifyMVar_ var $ \a0 -> do
  a <- action a0
  pure $! a
{-# INLINE modifyMVar_ #-}

-- | 'Control.Concurrent.MVar.modifyMVar' for a strict t'MVar'.
--
-- Evaluates the new value to WHNF. The returned value is not evaluated.
modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar (MVar var) action = Lazy.modifyMVar var $ \a0 -> do
  (a, b) <- action a0
  (, b) <$> (pure $! a)
{-# INLINE modifyMVar #-}

-- | 'Control.Concurrent.MVar.modifyMVarMasked_' for a strict t'MVar'.
--
-- Evaluates the new value to WHNF.
modifyMVarMasked_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVarMasked_ (MVar var) action = Lazy.modifyMVarMasked_ var $ \a0 -> do
  a <- action a0
  pure $! a
{-# INLINE modifyMVarMasked_ #-}

-- | 'Control.Concurrent.MVar.modifyMVarMasked' for a strict t'MVar'.
--
-- Evaluates the new value to WHNF. The returned value is not evaluated.
modifyMVarMasked :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVarMasked (MVar var) action = Lazy.modifyMVarMasked var $ \a0 -> do
  (a, b) <- action a0
  (, b) <$> (pure $! a)
{-# INLINE modifyMVarMasked #-}

-- | 'Control.Concurrent.MVar.mkWeakMVar' for a strict t'MVar'.
mkWeakMVar :: MVar a -> IO () -> IO (Weak (MVar a))
mkWeakMVar var@(MVar (GHC.MVar var#)) (IO finalizer) = IO $ \s0 ->
  case mkWeak# var# var finalizer s0 of
    (# s1, w #) -> (# s1, Weak w #)
