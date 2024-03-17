-- | For full documentation please refer to "Data.IORef".
module Data.IORef.Strict
  ( IORef
  , toStrictIORef
  , newIORef
  , readIORef
  , writeIORef
  , modifyIORef
  , atomicToIORef
  , atomicModifyIORef
  , atomicWriteIORef
  , mkWeakIORef
  ) where

import GHC.Exts (mkWeak#)
import GHC.IO (IO(..))
import GHC.STRef (STRef(..))
import GHC.Weak (Weak(..))
import qualified Data.IORef as Base
import qualified GHC.IORef as GHC (IORef(..))

-- | A strict (WHNF) variant of 'Base.IORef'.
newtype IORef a = IORef (Base.IORef a)
  deriving Eq

-- | Convert an 'Base.IORef' to a strict 'IORef'.
toStrictIORef :: Base.IORef a -> IO (IORef a)
toStrictIORef baseVar = do
  let var = IORef baseVar
  modifyIORef var id
  pure var

-- | 'Base.newIORef' for a strict 'IORef'.
newIORef :: a -> IO (IORef a)
newIORef !a = IORef <$> Base.newIORef a

-- | 'Base.readIORef' for a strict 'IORef'.
readIORef :: IORef a -> IO a
readIORef (IORef var) = Base.readIORef var

-- | 'Base.writeIORef' for a strict 'IORef'.
writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef var) !a = Base.writeIORef var a

-- | 'Base.modifyIORef' for a strict 'IORef'.
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef (IORef var) f = Base.modifyIORef' var f

-- | Convert an 'IORef' to a strict 'IORef' atomically.
atomicToIORef :: Base.IORef a -> IO (IORef a)
atomicToIORef baseVar = do
  let var = IORef baseVar
  atomicModifyIORef var (\a -> (a, ()))
  pure var

-- | 'Base.atomicModifyIORef' for a strict 'IORef'.
atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef (IORef var) f = Base.atomicModifyIORef' var f

-- | 'Base.atomicWriteIORef' for a strict 'IORef'.
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef (IORef var) !a = Base.atomicWriteIORef var a

-- | 'Base.mkWeakIORef' for a strict 'IORef'.
mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef var@(IORef (GHC.IORef (STRef var#))) (IO finalizer) = IO $ \s0 ->
  case mkWeak# var# var finalizer s0 of
    (# s1, w #) -> (# s1, Weak w #)
