-- | An @'MVar' t@ is a mutable location that is either empty or contains a
-- value of type @t@.  It has two fundamental operations: 'putMVar'
-- which fills an 'MVar' if it is empty and blocks otherwise, and
-- 'takeMVar' which empties an 'MVar' if it is full and blocks
-- otherwise.  They can be used in multiple different ways:
--
--   1. As synchronized mutable variables,
--
--   2. As channels, with 'takeMVar' and 'putMVar' as receive and send, and
--
--   3. As a binary semaphore @'MVar' ()@, with 'takeMVar' and 'putMVar' as
--      wait and signal.
--
-- They were introduced in the paper
-- ["Concurrent Haskell"](https://www.microsoft.com/en-us/research/wp-content/uploads/1996/01/concurrent-haskell.pdf)
-- by Simon Peyton Jones, Andrew Gordon and Sigbjorn Finne, though
-- some details of their implementation have since then changed (in
-- particular, a put on a full 'MVar' used to error, but now merely
-- blocks.)
--
-- === Applicability
--
-- 'MVar's offer more flexibility than t'Data.IORef.Strict.IORef's, but less
-- flexibility than t'GHC.Conc.STM'.  They are appropriate for building
-- synchronization primitives and performing simple inter-thread communication;
-- however they are very simple and susceptible to race conditions, deadlocks or
-- uncaught exceptions.  Do not use them if you need to perform larger
-- atomic operations such as reading from multiple variables: use t'GHC.Conc.STM'
-- instead.
--
-- In particular, the "bigger" functions in this module ('swapMVar',
-- 'withMVar', 'modifyMVar_' and 'modifyMVar') are simply
-- the composition of a 'takeMVar' followed by a 'putMVar' with
-- exception safety.
-- These have atomicity guarantees only if all other threads
-- perform a 'takeMVar' before a 'putMVar' as well;  otherwise, they may
-- block.
--
-- === Fairness
--
-- No thread can be blocked indefinitely on an 'MVar' unless another
-- thread holds that 'MVar' indefinitely.  One usual implementation of
-- this fairness guarantee is that threads blocked on an 'MVar' are
-- served in a first-in-first-out fashion (this is what GHC does),
-- but this is not guaranteed in the semantics.
--
-- === Ordering
--
-- 'MVar' operations are always observed to take place in the order
-- they are written in the program, regardless of the memory model of
-- the underlying machine.  This is in contrast to t'Data.IORef.Strict.IORef'
-- operations which may appear out-of-order to another thread in some cases.
--
-- === Example
--
-- Consider the following concurrent data structure, a skip channel.
-- This is a channel for an intermittent source of high bandwidth
-- information (for example, mouse movement events.)  Writing to the
-- channel never blocks, and reading from the channel only returns the
-- most recent value, or blocks if there are no new values.  Multiple
-- readers are supported with a @dupSkipChan@ operation.
--
-- A skip channel is a pair of 'MVar's. The first 'MVar' contains the
-- current value, and a list of semaphores that need to be notified
-- when it changes. The second 'MVar' is a semaphore for this particular
-- reader: it is full if there is a value in the channel that this
-- reader has not read yet, and empty otherwise.
--
-- @
-- data SkipChan a = SkipChan (MVar (a, [MVar ()])) (MVar ())
--
-- newSkipChan :: IO (SkipChan a)
-- newSkipChan = do
--     sem <- newEmptyMVar
--     main <- newMVar (undefined, [sem])
--     return (SkipChan main sem)
--
-- putSkipChan :: SkipChan a -> a -> IO ()
-- putSkipChan (SkipChan main _) v = do
--     (_, sems) <- takeMVar main
--     putMVar main (v, [])
--     mapM_ (\\sem -> putMVar sem ()) sems
--
-- getSkipChan :: SkipChan a -> IO a
-- getSkipChan (SkipChan main sem) = do
--     takeMVar sem
--     (v, sems) <- takeMVar main
--     putMVar main (v, sem : sems)
--     return v
--
-- dupSkipChan :: SkipChan a -> IO (SkipChan a)
-- dupSkipChan (SkipChan main _) = do
--     sem <- newEmptyMVar
--     (v, sems) <- takeMVar main
--     putMVar main (v, sem : sems)
--     return (SkipChan main sem)
-- @
--
-- This example was adapted from the original Concurrent Haskell paper.
-- For more examples of 'MVar's being used to build higher-level
-- synchronization primitives, see t'Control.Concurrent.Chan.Strict.Chan' and
-- t'Control.Concurrent.QSem.QSem'.
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
import Control.Exception (evaluate)
import GHC.Exts (mkWeak#)
import GHC.IO (IO(..))
import GHC.Weak (Weak(..))
import qualified Control.Concurrent.MVar as Base
import qualified GHC.MVar as GHC

-- | An 'MVar' (pronounced \"em-var\") is a synchronising variable
-- used for communication between concurrent threads, which evaluates its content
-- to Weak Head Normal Form.
-- It can be thought of as a box, which may be empty or full.
newtype MVar a = MVar (GHC.MVar a)
  deriving (Eq, NFData, NFData1)

-- | Create an 'MVar' which is initially empty.
newEmptyMVar :: IO (MVar a)
newEmptyMVar = MVar <$> Base.newEmptyMVar

-- | Create an 'MVar' which contains the supplied value.
--
-- Evaluates the initial value to WHNF.
newMVar :: a -> IO (MVar a)
newMVar a = fmap MVar . Base.newMVar =<< evaluate a

-- | Return the contents of the 'MVar'.  If the 'MVar' is currently
-- empty, 'takeMVar' will wait until it is full.  After a 'takeMVar',
-- the 'MVar' is left empty.
--
-- There are two further important properties of 'takeMVar':
--
--   * 'takeMVar' is single-wakeup.  That is, if there are multiple
--     threads blocked in 'takeMVar', and the 'MVar' becomes full,
--     only one thread will be woken up.  The runtime guarantees that
--     the woken thread completes its 'takeMVar' operation.
--
--   * When multiple threads are blocked on an 'MVar', they are
--     woken up in FIFO order.  This is useful for providing
--     fairness properties of abstractions built using 'MVar's.
--
takeMVar :: MVar a -> IO a
takeMVar (MVar var) = Base.takeMVar var

-- | Put a value into an 'MVar'.  If the 'MVar' is currently full,
-- 'putMVar' will wait until it becomes empty.
--
-- There are two further important properties of 'putMVar':
--
--   * 'putMVar' is single-wakeup.  That is, if there are multiple
--     threads blocked in 'putMVar', and the 'MVar' becomes empty,
--     only one thread will be woken up.  The runtime guarantees that
--     the woken thread completes its 'putMVar' operation.
--
--   * When multiple threads are blocked on an 'MVar', they are
--     woken up in FIFO order.  This is useful for providing
--     fairness properties of abstractions built using 'MVar's.
--
-- Evaluates the new value to WHNF.
putMVar :: MVar a -> a -> IO ()
putMVar (MVar var) a = Base.putMVar var =<< evaluate a

-- | Atomically read the contents of an 'MVar'.  If the 'MVar' is
-- currently empty, 'readMVar' will wait until it is full.
-- 'readMVar' is guaranteed to receive the next 'putMVar'.
--
-- 'readMVar' is multiple-wakeup, so when multiple readers are
-- blocked on an 'MVar', all of them are woken up at the same time.
-- The runtime guarantees that all woken threads complete their 'readMVar' operation.
readMVar :: MVar a -> IO a
readMVar (MVar var) = Base.readMVar var

-- | Take a value from an 'MVar', put a new value into the 'MVar' and
-- return the value taken. This function is atomic only if there are
-- no other producers for this 'MVar'. In other words, it cannot guarantee
-- that, by the time 'swapMVar' gets the chance to write to the 'MVar',
-- the value of the 'MVar' has not been altered
-- by a write operation from another thread.
--
-- Evaluates the new value to WHNF.
swapMVar :: MVar a -> a -> IO a
swapMVar (MVar var) a = Base.swapMVar var =<< evaluate a

-- | A non-blocking version of 'takeMVar'.  The 'tryTakeMVar' function
-- returns immediately, with 'Nothing' if the 'MVar' was empty, or
-- @'Just' a@ if the 'MVar' was full with contents @a@.  After 'tryTakeMVar',
-- the 'MVar' is left empty.
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar var) = Base.tryTakeMVar var

-- | A non-blocking version of 'putMVar'.  The 'tryPutMVar' function
-- attempts to put the value @a@ into the 'MVar', returning 'True' if
-- it was successful, or 'False' otherwise.
--
-- Evaluates the new value to WHNF.
tryPutMVar :: MVar a -> a -> IO Bool
tryPutMVar (MVar var) a = Base.tryPutMVar var =<< evaluate a

-- | A non-blocking version of 'readMVar'.  The 'tryReadMVar' function
-- returns immediately, with 'Nothing' if the 'MVar' was empty, or
-- @'Just' a@ if the 'MVar' was full with contents @a@.
tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar (MVar var) = Base.tryReadMVar var

-- | Check whether a given 'MVar' is empty.
--
-- Notice that the boolean value returned is just a snapshot of
-- the state of the 'MVar'. By the time you get to react on its result,
-- the 'MVar' may have been filled (or emptied) - so be extremely
-- careful when using this operation.  Use 'tryTakeMVar' instead if possible.
isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar (MVar var) = Base.isEmptyMVar var

-- | 'withMVar' is an exception-safe wrapper for operating on the contents
-- of an 'MVar'.  This operation is exception-safe: it will replace the
-- original contents of the 'MVar' if an exception is raised (see
-- "Control.Exception").  However, it is only atomic if there are no
-- other producers for this 'MVar'. In other words, it cannot guarantee
-- that, by the time 'withMVar' gets the chance to write to the 'MVar',
-- the value of the 'MVar' has not been altered
-- by a write operation from another thread.
withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar (MVar var) action = Base.withMVar var action
{-# INLINE withMVar #-}

-- | Like 'withMVar', but the @IO@ action in the second argument is executed
-- with asynchronous exceptions masked.
withMVarMasked :: MVar a -> (a -> IO b) -> IO b
withMVarMasked (MVar var) action = Base.withMVarMasked var action
{-# INLINE withMVarMasked #-}

-- | An exception-safe wrapper for modifying the contents of an 'MVar'.
-- Like 'withMVar', 'modifyMVar_' will replace the original contents of
-- the 'MVar' if an exception is raised during the operation.  This
-- function is only atomic if there are no other producers for this
-- 'MVar'. In other words, it cannot guarantee that, by the time
-- 'modifyMVar_' gets the chance to write to the 'MVar', the value
-- of the 'MVar' has not been altered by a write operation from another thread.
--
-- Evaluates the new value to WHNF.
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ (MVar var) action = Base.modifyMVar_ var $ \a0 -> do
  a <- action a0
  evaluate a
{-# INLINE modifyMVar_ #-}

-- | A slight variation on 'modifyMVar_' that allows a value to be
-- returned (@b@) in addition to the modified value of the 'MVar'.
--
-- Evaluates the new value to WHNF. The returned value is not evaluated.
modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar (MVar var) action = Base.modifyMVar var $ \a0 -> do
  (a, b) <- action a0
  (, b) <$> evaluate a
{-# INLINE modifyMVar #-}

-- | Like 'modifyMVar_', but the @IO@ action in the second argument is executed with
-- asynchronous exceptions masked.
--
-- Evaluates the new value to WHNF.
modifyMVarMasked_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVarMasked_ (MVar var) action = Base.modifyMVarMasked_ var $ \a0 -> do
  a <- action a0
  evaluate a
{-# INLINE modifyMVarMasked_ #-}

-- | Like 'modifyMVar', but the @IO@ action in the second argument is executed with
-- asynchronous exceptions masked.
--
-- Evaluates the new value to WHNF. The returned value is not evaluated.
modifyMVarMasked :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVarMasked (MVar var) action = Base.modifyMVarMasked var $ \a0 -> do
  (a, b) <- action a0
  (, b) <$> evaluate a
{-# INLINE modifyMVarMasked #-}

-- | Make a 'Weak' pointer to an 'MVar', using the second argument as
-- a finalizer to run when the 'MVar' is garbage-collected.
mkWeakMVar :: MVar a -> IO () -> IO (Weak (MVar a))
mkWeakMVar var@(MVar (GHC.MVar var#)) (IO finalizer) = IO $ \s0 ->
  case mkWeak# var# var finalizer s0 of
    (# s1, w #) -> (# s1, Weak w #)
