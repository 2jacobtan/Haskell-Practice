module NestedIO where

import Data.Time.Calendar
import Data.Time.Clock
import System.Random

huehue :: IO (Either (IO Int) (IO ()))
huehue = do
  t <- getCurrentTime
  let (_, _, dayOfMonth) =
        toGregorian (utctDay t)
  case not $ even dayOfMonth of
    True ->
      return $ Left randomIO
    False ->
      return $
        Right (putStrLn "no soup for you")

main = do
  blah <- huehue
  either id (>> return 42) blah