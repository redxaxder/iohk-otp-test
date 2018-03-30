module Options(Options(..)) where

import Options.Generic
import Data.Char

data Options = Options
  { sendFor :: Int
  , waitFor :: Int
  , withSeed :: Maybe Int
  } deriving (Generic, Show)

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers defaultModifiers { fieldNameModifier = camelTo '-' }

--from aeson
camelTo :: Char -> String -> String
camelTo c = map toLower . go2 . go1
    where go1 "" = ""
          go1 (x:u:l:xs) | isUpper u && isLower l = x : c : u : l : go1 xs
          go1 (x:xs) = x : go1 xs
          go2 "" = ""
          go2 (l:u:xs) | isLower l && isUpper u = l : c : u : go2 xs
          go2 (x:xs) = x : go2 xs
