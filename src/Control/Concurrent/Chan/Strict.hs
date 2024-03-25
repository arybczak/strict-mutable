-- | For full documentation please refer to "Control.Concurrent.Chan".
module Control.Concurrent.Chan.Strict
  ( Chan'
  , newChan'
  , writeChan'
  , readChan'
  , dupChan'
  , getChanContents'
  , writeList2Chan'
  ) where

import Control.Exception (evaluate)
import qualified Control.Concurrent.Chan as Base

-- | A strict (WHNF) variant of 'Base.Chan'.
newtype Chan' a = Chan' (Base.Chan a)
  deriving Eq

-- | 'Base.newChan' for 'Chan''.
newChan' :: IO (Chan' a)
newChan' = Chan' <$> Base.newChan

-- | 'Base.writeChan' for 'Chan''.
writeChan' :: Chan' a -> a -> IO ()
writeChan' (Chan' chan) a = Base.writeChan chan =<< evaluate a

-- | 'Base.readChan' for 'Chan''.
readChan' :: Chan' a -> IO a
readChan' (Chan' chan) = Base.readChan chan

-- | 'Base.dupChan' for 'Chan''.
dupChan' :: Chan' a -> IO (Chan' a)
dupChan' (Chan' chan) = Chan' <$> Base.dupChan chan

-- | 'Base.getChanContents' for 'Chan''.
getChanContents' :: Chan' a -> IO [a]
getChanContents' (Chan' chan) = Base.getChanContents chan

-- | 'Base.writeList2Chan' for 'Chan''.
writeList2Chan' :: Chan' a -> [a] -> IO ()
writeList2Chan' (Chan' chan) as = do
  evaluate (seqList as)
  Base.writeList2Chan chan as
  where
    seqList :: [a] -> ()
    seqList = \case
      a : rest -> a `seq` seqList rest
      [] -> ()
