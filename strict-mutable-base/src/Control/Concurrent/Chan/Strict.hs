-- | A strict variant of t'Control.Concurrent.Chan.Chan' that keeps its
-- contents in weak head normal form (WHNF).
--
-- For full documentation please refer to "Control.Concurrent.Chan".
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
import qualified Control.Concurrent.Chan as Lazy

-- | A strict (WHNF) variant of t'Control.Concurrent.Chan.Chan'.
newtype Chan a = Chan (Lazy.Chan a)
  deriving Eq

-- | 'Control.Concurrent.Chan.newChan' for a strict t'Chan'.
newChan :: IO (Chan a)
newChan = Chan <$> Lazy.newChan

-- | 'Control.Concurrent.Chan.writeChan' for a strict t'Chan'.
--
-- Evaluates the value to WHNF.
writeChan :: Chan a -> a -> IO ()
writeChan (Chan chan) a = Lazy.writeChan chan =<< evaluate a

-- | 'Control.Concurrent.Chan.readChan' for a strict t'Chan'.
readChan :: Chan a -> IO a
readChan (Chan chan) = Lazy.readChan chan

-- | 'Control.Concurrent.Chan.dupChan' for a strict t'Chan'.
dupChan :: Chan a -> IO (Chan a)
dupChan (Chan chan) = Chan <$> Lazy.dupChan chan

-- | 'Control.Concurrent.Chan.getChanContents' for a strict t'Chan'.
getChanContents :: Chan a -> IO [a]
getChanContents (Chan chan) = Lazy.getChanContents chan

-- | 'Control.Concurrent.Chan.writeList2Chan' for a strict t'Chan'.
--
-- Evaluates the values to WHNF.
writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan = mapM_ . writeChan
