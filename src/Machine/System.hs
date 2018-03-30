module Machine.System where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Foldable (fold)
import qualified Data.Map.Strict as M
import System.Random.MWC (toSeed)
import GHC.Generics (Generic)
import Data.Binary (Binary)

import Machine.PartialSum
import Machine.Local

type SystemState node = M.Map node PartialSum

initState :: Ord a => [a] -> SystemState a
initState xs = M.fromList $ zip xs (repeat mempty)

step :: Ord a => SystemState a -> (a, PartialSum) -> SystemState  a
step s (node, next) = M.insert node next s

score :: Ord a => SystemState a -> String
score state = let x = (fold . map snd . sortBy (comparing fst) . M.toList) state in
  concat ["There were ", show $ offset x, " entries and the sum is ", show $ total x] 
