{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Options.Generic (getRecord)
import Data.String (fromString)
import Data.Maybe (fromMaybe)

import System.IO


import System.Timeout (timeout)
import Data.IORef
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, forM, void)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import GHC.Generics (Generic)
import Data.Binary(Binary)
import Data.Time.Clock

import qualified System.Random.MWC as Random
import qualified Data.Vector.Unboxed as V
import qualified Data.Map.Strict as M

import Consensus.Protocol
import qualified Consensus.Follower as F
import qualified Consensus.Leader as L
import qualified Nodes as N
import qualified Machine
import Options
import Logging


host = "127.0.0.1" 
port = "10502"


spawnLocalTimed :: Int -> Process () -> Process ProcessId
spawnLocalTimed t x =  do
  pid <- spawnLocal x
  spawnLocal $ do
    liftIO $ threadDelay t -- milliseconds
    exit pid "timed out"
  pure pid

data FollowerInit = FollowerInit
  { leaderId :: ProcessId
  , loggerId :: ProcessId
  , seed :: Int
  , sendTime :: Int
  , waitTime :: Int
  , systemState :: Machine.SystemState NodeId
  } deriving (Generic)
instance Binary FollowerInit



runFollower :: FollowerInit -> Process ()
runFollower FollowerInit{..} = do
  node <- getSelfNode
  sumChannel <- liftIO $ newMVar mempty
  resultChannel <- liftIO $ newIORef (F.newState systemState)
  liftIO . forkIO . void . timeout t1 $ Machine.run seed sumChannel
  follower <- spawnLocalTimed (t1 + t2) $ F.follow loggerId leaderId Machine.step sumChannel resultChannel
  register "follower" follower 
  liftIO $ threadDelay t1
  logMessage Stderr loggerId "Generation phase finished"
  r <- liftIO $ timeout t2 $ getResult (t2 `div` 2) resultChannel
  let x = case r of
            Nothing -> ":("
            Just x -> "Result: " ++ Machine.score x 
  logMessage Stdout loggerId x
  where
   t1 = sendTime * 1000000
   t2 = waitTime * 1000000

getResult :: Int -> IORef (F.State (Machine.SystemState NodeId)) -> IO (Machine.SystemState NodeId)
getResult t stateRef = do
  threadDelay (t `div` 2)
  maybe <- F.check =<< readIORef stateRef
  case maybe of
    Just result -> pure result
    Nothing -> getResult (t `div` 2) stateRef

remotable ['runFollower]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable

main :: IO ()
main = do
  options <- getRecord (fromString "OTP Test Task")
  let sendTime = sendFor options
      waitTime = waitFor options
      seed = fromMaybe 0 (withSeed options)
  hPutStrLn stderr $ concat ["Sending for ", show sendTime, " seconds."]
  hPutStrLn stderr $ concat ["Waiting for ", show waitTime, " seconds."]
  hPutStrLn stderr $ concat ["Using seed ", show seed, "."]

  -- initialize rng
  gen <- Random.initialize $ V.singleton $ fromIntegral seed

  -- initialize network
  Right transport <- createTransport host port (const (host, port)) defaultTCPParameters
  leaderNode <- newLocalNode transport rtable
  nodes <- N.nodes transport rtable

  -- init state machine state
  let systemState = Machine.initState nodes

  -- start the logger
  loggerId <- forkProcess leaderNode logger

  void $ timeout ((sendTime + waitTime) * 1000000) $ runProcess leaderNode $ do
    leaderId <- getSelfPid
    -- start followers
    pids <- forM nodes $ \n -> do
       nodeSeed <- liftIO $ Random.uniform gen
       spawn n $ $(mkClosure 'runFollower) FollowerInit 
         { leaderId = leaderId
         , loggerId = loggerId
         , seed = nodeSeed
         , waitTime = waitTime
         , sendTime = sendTime
         , systemState = systemState
         }
    let leaderState = L.newState nodes :: L.State Machine.PartialSum
    L.lead loggerId leaderState
