module Consensus.Follower where

import Control.Distributed.Process (NodeId, ProcessId, expectTimeout, Process, usend, getSelfNode)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad(forever)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Control.Concurrent.MVar as M

import qualified Consensus.Protocol as P
import Consensus.LogSegment
import Logging


type Step a b = (b -> (NodeId, a) -> b)

data State b = State 
  { committedLocal :: Integer -- the name is aspirational. the current approach doesn't reliably get nodes to agree on the index.
  , lastApplied :: Integer
  , accum :: LogSegment b
  , lastRecievedTime :: UTCTime
  }

check :: State b -> IO (Maybe b)
check State{..} = flip fmap getCurrentTime $ \now ->
  let stale = diffUTCTime now lastRecievedTime > 0.5 -- half a second
  in  accum !? committedLocal

newState :: b -> State b
newState x = State
  { committedLocal = -1
  , lastApplied = -1
  , accum = singleton x
  , lastRecievedTime = posixSecondsToUTCTime 0
  }

follow :: Serializable a => ProcessId -> ProcessId -> Step a b -> M.MVar a -> IORef (State b) -> Process ()
follow loggerId leader step entriesRef stateRef = forever $ do
  handleLeaderUpdate loggerId step stateRef
  usend leader =<<
    P.FollowerUpdate 
    <$> getSelfNode
    <*> liftIO (lastApplied <$> readIORef stateRef)
    <*> liftIO (M.tryTakeMVar entriesRef)

handleLeaderUpdate :: Serializable a => ProcessId ->  Step a b -> IORef (State b) -> Process ()
handleLeaderUpdate loggerId step stateRef = expectTimeout timeout >>= \x -> case x of 
  Nothing -> pure ()
  Just P.LeaderUpdate{..} -> do
    State{..} <- liftIO $ readIORef stateRef
    --logMessage Stderr loggerId $ show committed
    now <- liftIO getCurrentTime
    let committed' = committed 
        newEntries = toList $ entries $ dropUntilIndex (lastApplied + 1) batch
        lastAccum = fromJust (lastLog accum)
        newAccums = drop 1 $ scanl step lastAccum newEntries
        accum' = appendEntries accum newAccums 
    liftIO $ writeIORef stateRef State
      { committedLocal = committed'
      , lastApplied = lastIndex accum' - 1
      , accum = accum'
      , lastRecievedTime = now
      }
  where
   timeout = 1000000 -- one second in microseconds
