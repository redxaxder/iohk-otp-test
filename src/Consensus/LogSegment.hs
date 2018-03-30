module Consensus.LogSegment where

import Data.Sequence (Seq, (|>), ViewR(..))
import qualified Data.Sequence as S
import GHC.Generics (Generic)
import Data.Binary(Binary)
import Data.Monoid ((<>))
import Control.Monad (guard)

data LogSegment a = LogSegment 
 { shiftAmount :: !Integer   -- if entries is non empty, this is the index of the first element.
                             -- if it is empty, the next element appended will have this index.
 , entries :: !(Seq a)
 } deriving (Generic, Show)

instance Binary a => Binary (LogSegment a)

nextIndex :: LogSegment a -> Integer
nextIndex (LogSegment shift e) = shift + fromIntegral (S.length e)

lastLog :: LogSegment a -> Maybe a
lastLog (LogSegment _ e) = case S.viewr e of
  EmptyR -> Nothing
  _ :> x -> Just x

takeLog :: Int -> LogSegment a -> LogSegment a
takeLog i (LogSegment s e) = LogSegment s (S.take i e)

(!?) :: LogSegment a -> Integer -> Maybe a
(LogSegment s e) !? i = e S.!? fromIntegral (i - s)

-- drops all entries with index less than i
dropUntilIndex :: Integer -> LogSegment a -> LogSegment a
dropUntilIndex i l = dropLog numToDrop l
  where numToDrop = i - shiftAmount l

dropLog :: Integer -> LogSegment a -> LogSegment a
dropLog n (LogSegment shiftAmount entries) = let trim = min (fromIntegral n) (S.length entries)
  in LogSegment (shiftAmount + max 0 (fromIntegral trim)) (S.drop trim entries)

appendLogEntry :: LogSegment a -> a -> LogSegment a
appendLogEntry (LogSegment ix entries) x = LogSegment ix (entries |> x)

appendEntries :: LogSegment a -> [a] -> LogSegment a
appendEntries (LogSegment s e) l = LogSegment s(e <> S.fromList l)

lastIndex :: LogSegment a -> Integer
lastIndex (LogSegment s e) = s + fromIntegral (length e)

singleton :: a -> LogSegment a
singleton x = LogSegment 0 (S.empty S.|> x)

emptyLog :: LogSegment a
emptyLog = LogSegment 0 S.empty
