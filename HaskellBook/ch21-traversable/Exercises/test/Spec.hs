import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (traversable)
import Lib
import Test.QuickCheck (arbitrary, Gen)
import Test.QuickCheck (sample)
-- import Test.QuickCheck (sample')
import Data.Functor ((<&>))

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
  quickBatch . traversable $ (undefined :: Identity (Int, Int, [Int]))
  putStrLn "Identity"
  quickBatch . traversable $ (undefined :: Constant Int (Int, Int, [Int]))
  putStrLn "Constant"
  quickBatch . traversable $ (undefined :: Optional (Int, Int, [Int]))
  putStrLn "Optional"
  quickBatch . traversable $ (undefined :: Three Int Int (Int, Int, [Int]))
  putStrLn "Three"
  quickBatch . traversable $ (undefined :: Pair Int (Int, Int, [Int]))
  putStrLn "Pair" 
  quickBatch . traversable $ (undefined :: Big Int (Int, Int, [Int]))
  putStrLn "Big"
  -- sample' (arbitrary :: Gen (S [] Int)) >>= print
  putStrLn "S sample'"
  sample (arbitrary :: Gen (S [] Int)) <&> print
  putStrLn "S sample"
  quickBatch . traversable $ (undefined :: S [] (Int, Int, [Int]))
  putStrLn "S"
  