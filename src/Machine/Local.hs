module Machine.Local where

import Machine.PartialSum
import qualified System.Random.MWC as Random
import qualified Control.Concurrent.MVar as M
import Data.IORef (newIORef, readIORef, modifyIORef, writeIORef)
import Control.Monad.ST (runST)
import Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime, picosecondsToDiffTime)
import Control.Monad (when, forever, void)
import Data.Monoid ((<>))
import qualified Data.Vector.Unboxed as V
import Control.Concurrent (threadDelay)

updateFrequency :: NominalDiffTime
updateFrequency = 0.5 -- half a second

run :: Int -> M.MVar PartialSum -> IO ()
run seed sharedState = do
  myState <- newIORef =<< M.readMVar sharedState
  gen <- Random.initialize $ V.singleton $ fromIntegral seed
  lastUpdate <- newIORef =<< getCurrentTime
  forever $ do
    threadDelay 10
    nextNumber <- fromDouble <$> Random.uniformR (0,1) gen
    modifyIORef myState (<>nextNumber)
    elapsed <- diffUTCTime <$> getCurrentTime <*> readIORef lastUpdate 
    when (elapsed > updateFrequency) $ do
      void $ M.tryPutMVar sharedState =<< readIORef myState
      writeIORef lastUpdate =<< getCurrentTime
