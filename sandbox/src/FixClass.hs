{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module FixClass where

data Nat = Z | S Nat

class Fix (n::Nat) where
    fix :: (a -> a) -> a

instance Fix Z where
  fix f = f undefined

instance Fix n => Fix (S n) where
  fix f = f (fix @n f)

sum' :: ([Int] -> Int) -> [Int] -> Int
sum' loop = g loop
  where
  g loop rest = case rest of 
    [] -> 0
    x:xs -> x + loop xs

x = fix @(  (S (S Z))) sum' [1..3]
y = fix @(S (S (S Z))) sum' [1..3]
