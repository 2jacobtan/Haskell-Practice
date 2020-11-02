import Test.QuickCheck.Checkers (quickBatch)
import Test.QuickCheck.Classes (traversable)
import Lib

-- type TI = []

-- main = do
--   -- let trigger :: TI (Int, Int, [Int])
--       trigger = undefined
--   quickBatch (traversable trigger)

main :: IO ()
main = do
  -- quickBatch . traversable $ (undefined :: [] (Int, Int, [Int]))
  -- putStrLn "[]"
  -- quickBatch . traversable $ (undefined :: Maybe (Int, Int, [Int]))
  -- putStrLn "Maybe"
  quickBatch . traversable $ (undefined :: List (Int, Int, [Int]))
  putStrLn "List"
