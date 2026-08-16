-- | Mutable references in the IO monad.
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
import Control.Exception (evaluate)
import GHC.Exts (mkWeak#)
import GHC.IO (IO(..))
import GHC.STRef (STRef(..))
import GHC.Weak (Weak(..))
import qualified Data.IORef as Base
import qualified GHC.IORef as GHC

-- | A mutable variable in the @IO@ monad, which evaluates its content to Weak
-- Head Normal Form.
newtype IORef a = IORef (Base.IORef a)
  deriving (Eq, NFData, NFData1)

-- | Build a new 'IORef'.
--
-- Evaluates the initial value to WHNF.
newIORef :: a -> IO (IORef a)
newIORef a = fmap IORef . Base.newIORef =<< evaluate a

-- | Read the value of an 'IORef'.
--
-- Beware that the CPU executing a thread can reorder reads or writes
-- to independent locations. See "Data.IORef#memmodel" for more details.
readIORef :: IORef a -> IO a
readIORef (IORef var) = Base.readIORef var

-- | Write a new value into an 'IORef'.
--
-- This function does not create a memory barrier and can be reordered
-- with other independent reads and writes within a thread, which may cause issues
-- for multithreaded execution. In these cases, consider using 'atomicWriteIORef'
-- instead. See "Data.IORef#memmodel" for more details.
--
-- Evaluates the new value to WHNF.
writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef var) a = Base.writeIORef var =<< evaluate a

-- | Mutate the contents of an 'IORef', combining 'readIORef' and 'writeIORef'.
-- This is not an atomic update, consider using 'atomicModifyIORef' when
-- operating in a multithreaded environment.
--
-- Evaluates the new value to WHNF.
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef (IORef var) f = Base.modifyIORef' var f

-- | Atomically modifies the contents of an 'IORef'.
--
-- This function is useful for using 'IORef' in a safe way in a multithreaded
-- program.  If you only have one 'IORef', then using 'atomicModifyIORef' to
-- access and modify it will prevent race conditions.
--
-- Extending the atomicity to multiple 'IORef's is problematic, so it
-- is recommended that if you need to do anything more complicated
-- then using t'Control.Concurrent.MVar.Strict.MVar' instead is a good idea.
--
-- Conceptually,
--
-- @
-- atomicModifyIORef ref f = do
--   -- Begin atomic block
--   old <- 'readIORef' ref
--   let r = f old
--       new = fst r
--   'writeIORef' ref new
--   -- End atomic block
--   case r of
--     (_new, res) -> pure res
-- @
--
-- The actions in the section labeled \"atomic block\" are not subject to
-- interference from other threads. In particular, it is impossible for the
-- value in the 'IORef' to change between the 'readIORef' and 'writeIORef'
-- invocations.
--
-- Note that
--
-- @atomicModifyIORef ref (\\_ -> undefined)@
--
-- will raise an exception in the calling thread, but will /also/
-- install the bottoming value in the 'IORef', where it may be read by
-- other threads.
--
-- This function imposes a memory barrier, preventing reordering around the
-- \"atomic block\"; see "Data.IORef#memmodel" for details.
--
-- Evaluates both the new value and the returned value to WHNF.
atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef (IORef var) f = Base.atomicModifyIORef' var f

-- | Variant of 'writeIORef'. The prefix "atomic" relates to a fact that
-- it imposes a reordering barrier, similar to 'atomicModifyIORef'.
-- Such a write will not be reordered with other reads
-- or writes even on CPUs with weak memory model.
--
-- Evaluates the new value to WHNF.
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef (IORef var) a = Base.atomicWriteIORef var =<< evaluate a

-- | Make a 'Weak' pointer to an 'IORef', using the second argument as a finalizer
-- to run when the 'IORef' is garbage-collected.
mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef var@(IORef (GHC.IORef (STRef var#))) (IO finalizer) = IO $ \s0 ->
  case mkWeak# var# var finalizer s0 of
    (# s1, w #) -> (# s1, Weak w #)
