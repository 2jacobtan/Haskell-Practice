{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module TypeclassMetaprogramming where

-- Alexis' version
type family ElementOf a where
  ElementOf [[a]] = ElementOf [a]
  ElementOf [a] = a

class Flatten a where
  flatten :: a -> [ElementOf a]

instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  flatten = flatten . concat

instance ElementOf [a] ~ a => Flatten [a] where
  flatten x = x

-- my version
type family Flat a where
  Flat [[a]] = Flat [a]
  Flat [a] = [a]

class Flatten' a where
  flatten' :: a -> Flat a

instance {-# OVERLAPPING #-} Flatten' [a] => Flatten' [[a]] where
  flatten' = flatten' . concat

instance Flat [a] ~ [a] => Flatten' [a] where
  flatten' x = x

main :: IO ()
main = do
  let x = [[1 :: Int],[2]]
  print $ flatten x
  print $ flatten' x
  let y = [3 :: Int, 4]
  print $ flatten y
  print $ flatten' y
  let z = [[[5 :: Int]],[[6]]]
  print $ flatten z
  print $ flatten' z