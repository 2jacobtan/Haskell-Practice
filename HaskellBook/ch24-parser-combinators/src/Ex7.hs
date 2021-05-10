module Ex6 where

import Data.Word

data IPAddress6 =
  IPAddress Word64 Word64
  deriving (Eq, Ord, Show)

