module Criterion where

import Criterion.Main

-- import MyLib

myList :: [Int]
myList = [1 .. 9999]

main :: IO ()
main =
  defaultMain
    [ bench "index list 9999" $
        whnf (myList !!) 9998,
      bench "index list maybe index 9999" $
        whnf (myList !?) 9998
    ]

-- | first version, adapted from reference implementation
-- infixl 9 !?
-- _ !? n | n < 0 = Nothing
-- [] !? _ = Nothing
-- (x : _) !? 0 = Just x
-- (_ : xs) !? n = xs !? (n -1)

-- | second version, adapted from official implementation
infixl 9 !?

{-# INLINEABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
    foldr
      ( \x r k ->
          case k of
            0 -> Just x
            _ -> r (k -1)
      )
      (const Nothing) xs n
