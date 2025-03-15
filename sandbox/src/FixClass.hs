{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module FixClass where

data Nat = Z | S Nat

class Fix (n::Nat) where
    fixDef :: a -> (a -> a) -> a

instance Fix Z where
  fixDef d f = d

instance Fix n => Fix (S n) where
  fixDef d f = f (fixDef @n d f)

sum' :: ([Int] -> Int) -> [Int] -> Int
sum' rec rest = case rest of
  [] -> 0
  x:xs -> x + rec xs

x = fixDef @(  (S (S Z))) (const 0) sum' [1..3]
y = fixDef @(S (S (S Z))) (const 0) sum' [1..3]

fixDef' 0 d f = d
fixDef' n d f = f (fixDef' (n-1) d f)

x' = fixDef' 2 (const 0) sum' [1..3]
y' = fixDef' 3 (const 0) sum' [1..3]
