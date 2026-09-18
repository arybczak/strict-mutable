-- | A strict variant of t'Data.IORef.IORef' that keeps its contents in
-- weak head normal form (WHNF).
--
-- For full documentation please refer to "Data.IORef".
module Data.IORef.Strict
  ( IORef

    -- * Operations
  , newIORef
  , readIORef
  , writeIORef
  , modifyIORef
  , atomicModifyIORef
  , atomicWriteIORef
  , mkWeakIORef
  ) where

import Control.DeepSeq
import GHC.Exts (mkWeak#)
import GHC.IO (IO(..))
import GHC.STRef (STRef(..))
import GHC.Weak (Weak(..))
import qualified Data.IORef as Lazy
import qualified GHC.IORef as GHC

-- | A strict (WHNF) variant of t'Data.IORef.IORef'.
newtype IORef a = IORef (Lazy.IORef a)
  deriving (Eq, NFData, NFData1)

-- | 'Data.IORef.newIORef' for a strict t'IORef'.
--
-- Evaluates the initial value to WHNF.
newIORef :: a -> IO (IORef a)
newIORef a = fmap IORef . Lazy.newIORef =<< (pure $! a)

-- | 'Data.IORef.readIORef' for a strict t'IORef'.
readIORef :: IORef a -> IO a
readIORef (IORef var) = Lazy.readIORef var

-- | 'Data.IORef.writeIORef' for a strict t'IORef'.
--
-- Evaluates the new value to WHNF.
writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef var) a = Lazy.writeIORef var =<< (pure $! a)

-- | 'Data.IORef.modifyIORef' for a strict t'IORef'.
--
-- Evaluates the new value to WHNF.
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef (IORef var) f = Lazy.modifyIORef' var f

-- | 'Data.IORef.atomicModifyIORef' for a strict t'IORef'.
--
-- Evaluates both the new value and the returned value to WHNF.
atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef (IORef var) f = Lazy.atomicModifyIORef' var f

-- | 'Data.IORef.atomicWriteIORef' for a strict t'IORef'.
--
-- Evaluates the new value to WHNF.
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef (IORef var) a = Lazy.atomicWriteIORef var =<< (pure $! a)

-- | 'Data.IORef.mkWeakIORef' for a strict t'IORef'.
mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef var@(IORef (GHC.IORef (STRef var#))) (IO finalizer) = IO $ \s0 ->
  case mkWeak# var# var finalizer s0 of
    (# s1, w #) -> (# s1, Weak w #)
