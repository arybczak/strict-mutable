-- | Unbounded channels whose content is evaluated to Weak Head Normal Form (WHNF).
--
-- The channels are implemented with t'Control.Concurrent.MVar.MVar's and
-- therefore inherit all the caveats that apply to @MVar@s (possibility of
-- races, deadlocks etc). The
-- @stm@ (software transactional memory) library has a more robust implementation
-- of channels called @TChan@s.
module Control.Concurrent.Chan.Strict
  ( Chan

    -- * Operations
  , newChan
  , writeChan
  , readChan
  , dupChan
  , getChanContents
  , writeList2Chan
  ) where

import Control.Exception (evaluate)
import qualified Control.Concurrent.Chan as Base

-- | 'Chan' is a strict (WHNF) abstract type representing an unbounded FIFO channel.
newtype Chan a = Chan (Base.Chan a)
  deriving Eq

-- | Build and return a new instance of 'Chan'.
newChan :: IO (Chan a)
newChan = Chan <$> Base.newChan

-- | Write a value to a 'Chan'.
--
-- Evaluates the value to WHNF.
writeChan :: Chan a -> a -> IO ()
writeChan (Chan chan) a = Base.writeChan chan =<< evaluate a

-- | Read the next value from the 'Chan'. Blocks when the channel is empty. Since
-- the read end of a channel is an t'Control.Concurrent.MVar.MVar', this
-- operation inherits fairness guarantees of @MVar@s (e.g. threads blocked in
-- this operation are woken up in FIFO order).
--
-- Throws t'Control.Exception.BlockedIndefinitelyOnMVar' when the channel is
-- empty and no other thread holds a reference to the channel.
readChan :: Chan a -> IO a
readChan (Chan chan) = Base.readChan chan

-- | Duplicate a 'Chan': the duplicate channel begins empty, but data written to
-- either channel from then on will be available from both. Hence this creates
-- a kind of broadcast channel, where data written by anyone is seen by
-- everyone else.
--
-- (Note that a duplicated channel is not equal to its original.
-- So: @fmap (c /=) $ dupChan c@ returns 'True' for all @c@.)
dupChan :: Chan a -> IO (Chan a)
dupChan (Chan chan) = Chan <$> Base.dupChan chan

-- | Return a lazy list representing the contents of the supplied 'Chan', much
-- like 'System.IO.hGetContents'.
getChanContents :: Chan a -> IO [a]
getChanContents (Chan chan) = Base.getChanContents chan

-- | Write an entire list of items to a 'Chan'.
--
-- Evaluates the values to WHNF.
writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan = mapM_ . writeChan
