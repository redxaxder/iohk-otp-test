module Nodes where

import Control.Monad
import Control.Distributed.Process (NodeId, RemoteTable)
import Control.Distributed.Process.Node

import  Network.Transport (Transport)

nodes :: Transport -> RemoteTable -> IO [NodeId]
nodes t rtable = forM [1..5] $ \_ -> localNodeId <$> newLocalNode t rtable
