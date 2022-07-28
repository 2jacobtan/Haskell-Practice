{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
module Lib
    ( someFunc
    , x) where
import Data.Char (intToDigit)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | right fold [Int] alongside left fold String
x :: ([Int],String)
x = foldr f ([],) [1..4] ""
  where 
    f :: Int -> (String -> ([Int],String)) -> String -> ([Int],String)
    f l r acc =
      let (r',a') = r (intToDigit l : acc)
      in (l : r', a')  -- (right fold result, left fold result)

