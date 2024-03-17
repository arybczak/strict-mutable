-- | For full documentation please refer to "Control.Concurrent.MVar".
module Control.Concurrent.MVar.Strict
  ( MVar
  , toStrictMVar
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

import GHC.Exts (mkWeak#)
import GHC.IO (IO(..))
import GHC.Weak (Weak(..))
import qualified Control.Concurrent.MVar as Base
import qualified GHC.MVar as GHC (MVar(..))

-- | Strict (WHNF) version of 'Base.MVar'.
newtype MVar a = MVar (Base.MVar a)
  deriving Eq

-- | Convert an 'Base.MVar' to a strict 'MVar'.
toStrictMVar :: Base.MVar a -> IO (MVar a)
toStrictMVar baseVar = do
  let var = MVar baseVar
  modifyMVar_ var pure
  pure var

-- | 'Base.newEmptyMVar' for a strict 'MVar'.
newEmptyMVar :: IO (MVar a)
newEmptyMVar = MVar <$> Base.newEmptyMVar

-- | 'Base.newMVar' for a strict 'MVar'.
newMVar :: a -> IO (MVar a)
newMVar !a = MVar <$> Base.newMVar a

-- | 'Base.takeMVar' for a strict 'MVar'.
takeMVar :: MVar a -> IO a
takeMVar (MVar var) = Base.takeMVar var

-- | 'Base.putMVar' for a strict 'MVar'.
putMVar :: MVar a -> a -> IO ()
putMVar (MVar var) !a = Base.putMVar var a

-- | 'Base.readMVar' for a strict 'MVar'.
readMVar :: MVar a -> IO a
readMVar (MVar var) = Base.readMVar var

-- | 'Base.swapMVar' for a strict 'MVar'.
swapMVar :: MVar a -> a -> IO a
swapMVar (MVar var) !a = Base.swapMVar var a

-- | 'Base.tryTakeMVar' for a strict 'MVar'.
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar var) = Base.tryTakeMVar var

-- | 'Base.tryPutMVar' for a strict 'MVar'.
tryPutMVar :: MVar a -> a -> IO Bool
tryPutMVar (MVar var) !a = Base.tryPutMVar var a

-- | 'Base.tryReadMVar' for a strict 'MVar'.
tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar (MVar var) = Base.tryReadMVar var

-- | 'Base.isEmptyMVar' for a strict 'MVar'.
isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar (MVar var) = Base.isEmptyMVar var

-- | 'Base.withMVar' for a strict 'MVar'.
withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar (MVar var) action = Base.withMVar var action
{-# INLINE withMVar #-}

-- | 'Base.withMVarMasked' for a strict 'MVar'.
withMVarMasked :: MVar a -> (a -> IO b) -> IO b
withMVarMasked (MVar var) action = Base.withMVarMasked var action
{-# INLINE withMVarMasked #-}

-- | 'Base.modifyMVar_' for a strict 'MVar'.
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ (MVar var) action = Base.modifyMVar_ var $ \a0 -> do
  a <- action a0
  a `seq` pure a
{-# INLINE modifyMVar_ #-}

-- | 'Base.modifyMVar' for a strict 'MVar'.
modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar (MVar var) action = Base.modifyMVar var $ \a0 -> do
  (a, b) <- action a0
  a `seq` pure (a, b)
{-# INLINE modifyMVar #-}

-- | 'Base.modifyMVarMasked_' for a strict 'MVar'.
modifyMVarMasked_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVarMasked_ (MVar var) action = Base.modifyMVarMasked_ var $ \a0 -> do
  a <- action a0
  a `seq` pure a
{-# INLINE modifyMVarMasked_ #-}

-- | 'Base.modifyMVarMasked' for a strict 'MVar'.
modifyMVarMasked :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVarMasked (MVar var) action = Base.modifyMVarMasked var $ \a0 -> do
  (a, b) <- action a0
  a `seq` pure (a, b)
{-# INLINE modifyMVarMasked #-}

-- | 'Base.mkWeakMVar' for a strict 'MVar'.
mkWeakMVar :: MVar a -> IO () -> IO (Weak (MVar a))
mkWeakMVar var@(MVar (GHC.MVar var#)) (IO finalizer) = IO $ \s0 ->
  case mkWeak# var# var finalizer s0 of
    (# s1, w #) -> (# s1, Weak w #)
