-- | For full documentation please refer to "Control.Concurrent.MVar".
module Control.Concurrent.MVar.Strict
  ( MVar'
  , toMVar'
  , newEmptyMVar'
  , newMVar'
  , takeMVar'
  , putMVar'
  , readMVar'
  , swapMVar'
  , tryTakeMVar'
  , tryPutMVar'
  , tryReadMVar'
  , isEmptyMVar'
  , withMVar'
  , withMVarMasked'
  , modifyMVar_'
  , modifyMVar'
  , modifyMVarMasked_'
  , modifyMVarMasked'
  , mkWeakMVar'
  ) where

import qualified Control.Concurrent.MVar as Base
import GHC.Exts (mkWeak#)
import GHC.IO (IO(..))
import GHC.MVar (MVar(..))
import GHC.Weak (Weak(..))

-- | Strict (WHNF) version of 'MVar'.
newtype MVar' a = MVar' (MVar a)
  deriving Eq

-- | Convert an 'MVar' to an 'MVar''.
toMVar' :: MVar a -> IO (MVar' a)
toMVar' var = do
  let var' = MVar' var
  modifyMVar_' var' pure
  pure var'

-- | 'Base.newEmptyMVar' for an 'MVar''.
newEmptyMVar' :: IO (MVar' a)
newEmptyMVar' = MVar' <$> Base.newEmptyMVar

-- | 'Base.newMVar' for an 'MVar''.
newMVar' :: a -> IO (MVar' a)
newMVar' !a = MVar' <$> Base.newMVar a

-- | 'Base.takeMVar' for an 'MVar''.
takeMVar' :: MVar' a -> IO a
takeMVar' (MVar' var) = Base.takeMVar var

-- | 'Base.putMVar' for an 'MVar''.
putMVar' :: MVar' a -> a -> IO ()
putMVar' (MVar' var) !a = Base.putMVar var a

-- | 'Base.readMVar' for an 'MVar''.
readMVar' :: MVar' a -> IO a
readMVar' (MVar' var) = Base.readMVar var

-- | 'Base.swapMVar' for an 'MVar''.
swapMVar' :: MVar' a -> a -> IO a
swapMVar' (MVar' var) !a = Base.swapMVar var a

-- | 'Base.tryTakeMVar' for an 'MVar''.
tryTakeMVar' :: MVar' a -> IO (Maybe a)
tryTakeMVar' (MVar' var) = Base.tryTakeMVar var

-- | 'Base.tryPutMVar' for an 'MVar''.
tryPutMVar' :: MVar' a -> a -> IO Bool
tryPutMVar' (MVar' var) !a = Base.tryPutMVar var a

-- | 'Base.tryReadMVar' for an 'MVar''.
tryReadMVar' :: MVar' a -> IO (Maybe a)
tryReadMVar' (MVar' var) = Base.tryReadMVar var

-- | 'Base.isEmptyMVar' for an 'MVar''.
isEmptyMVar' :: MVar' a -> IO Bool
isEmptyMVar' (MVar' var) = Base.isEmptyMVar var

-- | 'Base.withMVar' for an 'MVar''.
withMVar' :: MVar' a -> (a -> IO b) -> IO b
withMVar' (MVar' var) action = Base.withMVar var action
{-# INLINE withMVar' #-}

-- | 'Base.withMVarMasked' for an 'MVar''.
withMVarMasked' :: MVar' a -> (a -> IO b) -> IO b
withMVarMasked' (MVar' var) action = Base.withMVarMasked var action
{-# INLINE withMVarMasked' #-}

-- | 'Base.modifyMVar_' for an 'MVar''.
modifyMVar_' :: MVar' a -> (a -> IO a) -> IO ()
modifyMVar_' (MVar' var) action = Base.modifyMVar_ var $ \a0 -> do
  a <- action a0
  a `seq` pure a
{-# INLINE modifyMVar_' #-}

-- | 'Base.modifyMVar' for an 'MVar''.
modifyMVar' :: MVar' a -> (a -> IO (a, b)) -> IO b
modifyMVar' (MVar' var) action = Base.modifyMVar var $ \a0 -> do
  (a, b) <- action a0
  a `seq` pure (a, b)
{-# INLINE modifyMVar' #-}

-- | 'Base.modifyMVarMasked_' for an 'MVar''.
modifyMVarMasked_' :: MVar' a -> (a -> IO a) -> IO ()
modifyMVarMasked_' (MVar' var) action = Base.modifyMVarMasked_ var $ \a0 -> do
  a <- action a0
  a `seq` pure a
{-# INLINE modifyMVarMasked_' #-}

-- | 'Base.modifyMVarMasked' for an 'MVar''.
modifyMVarMasked' :: MVar' a -> (a -> IO (a, b)) -> IO b
modifyMVarMasked' (MVar' var) action = Base.modifyMVarMasked var $ \a0 -> do
  (a, b) <- action a0
  a `seq` pure (a, b)
{-# INLINE modifyMVarMasked' #-}

-- | 'Base.mkWeakMVar' for an 'MVar''.
mkWeakMVar' :: MVar' a -> IO () -> IO (Weak (MVar' a))
mkWeakMVar' var@(MVar' (MVar var#)) (IO finalizer) = IO $ \s0 ->
  case mkWeak# var# var finalizer s0 of
    (# s1, w #) -> (# s1, Weak w #)
