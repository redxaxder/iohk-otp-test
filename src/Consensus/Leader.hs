module Consensus.Leader where

import Control.Distributed.Process (spawnLocal, NodeId, ProcessId, expectTimeout, Process, nsendRemote, getSelfPid)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad (forever, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Binary (Binary)
import Data.List (sort)
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as M
import Data.IORef
import Control.Concurrent (threadDelay)
import Data.Time.Clock

import qualified Consensus.Protocol as P
import Consensus.LogSegment

import Logging

data State a = State 
  { followers :: !(M.Map NodeId Integer) -- Last applied index of each follower
  , committed :: !Integer
  , records :: !(LogSegment (NodeId, a))
  } deriving Show

newState :: [NodeId] -> State a
newState pids = State (M.fromList $ map (\x -> (x,0)) pids) 0 emptyLog

lead :: Serializable a => ProcessId -> State a -> Process ()
lead loggerId state = do
  stateRef <- liftIO $ newIORef state
  pid <- getSelfPid
  spawnLocal $ forever $ do
    s <- liftIO (readIORef stateRef) 
    sendLeaderUpdates loggerId s
    liftIO $ threadDelay 300000
  forever $ handleFollowerUpdate loggerId stateRef

--we mark an index as committed if we know that at least half of the followers have applied it
calcCommitted :: M.Map a Integer -> Integer
calcCommitted xs = sort (map snd (M.toList xs)) !! max 0 ((M.size xs `div` 2) - 1)

handleFollowerUpdate :: Serializable a => ProcessId -> IORef (State a) -> Process ()
handleFollowerUpdate loggerId stateRef = expectTimeout timeout >>= \x -> case x of
  Nothing -> pure ()
  Just u@P.FollowerUpdate{..} -> liftIO $ modifyIORef stateRef $ \State{..} ->
      let followers' = M.insert followerId lastApplied followers
          committed' = calcCommitted followers'
          records' = maybe records (\m -> appendLogEntry records (followerId, m)) message
      in  State followers' committed' records'
  where
  timeout = 1000000 -- one second in microseconds

sendLeaderUpdates :: Serializable a => ProcessId -> State a -> Process ()
sendLeaderUpdates loggerId State{..} = forM_ (M.toList followers) $ \(nid, applied) ->
  let entriesToSend = takeLog 1000 $ dropUntilIndex (applied + 1) records
  in  nsendRemote nid "follower" =<< P.LeaderUpdate 
        <$> getSelfPid 
        <*> pure committed 
        <*> pure entriesToSend
