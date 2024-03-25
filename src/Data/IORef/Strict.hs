-- | For full documentation please refer to "Data.IORef".
module Data.IORef.Strict
  ( IORef'
  , toIORef'
  , newIORef'
  , readIORef'
  , writeIORef'
  , modifyIORef'
  , atomicToIORef'
  , atomicModifyIORef'
  , atomicWriteIORef'
  , mkWeakIORef'
  ) where

import Control.DeepSeq
import Control.Exception (evaluate)
import GHC.Exts (mkWeak#)
import GHC.IO (IO(..))
import GHC.IORef (IORef(..))
import GHC.STRef (STRef(..))
import GHC.Weak (Weak(..))
import qualified Data.IORef as Base

-- | A strict (WHNF) variant of 'IORef'.
newtype IORef' a = IORef' (Base.IORef a)
  deriving (Eq, NFData, NFData1)

-- | Convert an 'IORef' to an 'IORef''.
toIORef' :: IORef a -> IO (IORef' a)
toIORef' var = do
  let var' = IORef' var
  modifyIORef' var' id
  pure var'

-- | 'Base.newIORef' for 'IORef''.
newIORef' :: a -> IO (IORef' a)
newIORef' a = fmap IORef' . Base.newIORef =<< evaluate a

-- | 'Base.readIORef' for 'IORef''.
readIORef' :: IORef' a -> IO a
readIORef' (IORef' var) = Base.readIORef var

-- | 'Base.writeIORef' for 'IORef''.
writeIORef' :: IORef' a -> a -> IO ()
writeIORef' (IORef' var) a = Base.writeIORef var =<< evaluate a

-- | 'Base.modifyIORef' for 'IORef''.
modifyIORef' :: IORef' a -> (a -> a) -> IO ()
modifyIORef' (IORef' var) f = Base.modifyIORef' var f

-- | Convert an 'IORef' to an 'IORef'' atomically.
atomicToIORef' :: IORef a -> IO (IORef' a)
atomicToIORef' var = do
  let var' = IORef' var
  atomicModifyIORef' var' (\a -> (a, ()))
  pure var'

-- | 'Base.atomicModifyIORef' for 'IORef''.
atomicModifyIORef' :: IORef' a -> (a -> (a, b)) -> IO b
atomicModifyIORef' (IORef' var) f = Base.atomicModifyIORef' var f

-- | 'Base.atomicWriteIORef' for 'IORef''.
atomicWriteIORef' :: IORef' a -> a -> IO ()
atomicWriteIORef' (IORef' var) a = Base.atomicWriteIORef var =<< evaluate a

-- | 'Base.mkWeakIORef' for 'IORef''.
mkWeakIORef' :: IORef' a -> IO () -> IO (Weak (IORef' a))
mkWeakIORef' var@(IORef' (IORef (STRef var#))) (IO finalizer) = IO $ \s0 ->
  case mkWeak# var# var finalizer s0 of
    (# s1, w #) -> (# s1, Weak w #)
