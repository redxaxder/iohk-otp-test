module Logging where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Control.Distributed.Process
import Control.Monad (forever)
import System.IO

logger :: Process ()
logger = forever $ do
  (Say c pid s) <- expect
  liftIO $ hPutStrLn (handle c) (show pid ++ " " ++ s)

handle :: Channel -> Handle
handle Stdout = stdout
handle Stderr = stderr

logMessage :: Channel -> ProcessId -> String -> Process ()
logMessage c printer s = do
  pid <- getSelfPid
  usend printer (Say c pid s)

data Channel = Stdout | Stderr deriving Generic
instance Binary Channel

data Say = Say Channel ProcessId String deriving Generic
instance Binary Say
