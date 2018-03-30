module Consensus.Protocol where

import Control.Distributed.Process (ProcessId, NodeId)
import Data.Monoid ((<>))
import Control.Monad (guard)
import GHC.Generics (Generic)
import Data.Binary (Binary)

import Consensus.LogSegment

data LeaderUpdate a = LeaderUpdate 
  { leaderId :: ProcessId
  , committed :: Integer
  , batch :: LogSegment a
  } deriving (Generic, Show)

instance Binary a => Binary (LeaderUpdate a)

data FollowerUpdate a = FollowerUpdate
  { followerId :: NodeId
  , lastApplied :: Integer
  , message :: Maybe a
  } deriving (Generic, Show)

instance Binary a => Binary (FollowerUpdate a)


