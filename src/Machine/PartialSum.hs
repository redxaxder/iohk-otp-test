module Machine.PartialSum where

import Data.Fixed (Pico)
import GHC.Generics (Generic)
import Data.Binary (Binary)

-- Using a fixed point type instead of Double makes the monoid instance associative.
data PartialSum = PartialSum { offset :: !Integer, shift :: !Pico, total :: !Pico }
  deriving (Eq, Ord, Show, Generic)

instance Binary PartialSum

fromDouble :: Double -> PartialSum
fromDouble d = let p = fromRational $ toRational d in
   PartialSum { offset = 1, shift = p, total = p }

instance Monoid PartialSum where
  mempty = PartialSum 0 0 0
  mappend a b =
    PartialSum {
      offset = offset a + offset b,
      shift = shift a + shift b,
      total = total a + (fromIntegral (offset a) * shift b) + total b
    }
