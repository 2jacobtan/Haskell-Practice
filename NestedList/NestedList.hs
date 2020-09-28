
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}

module NestedList (proptest) where

import Test.SmallCheck.Series
import Test.SmallCheck
import GHC.Generics

data NestedList a = Nil
  | Value a
  | Cons (NestedList a) (NestedList a)
  deriving (Show, Eq, Generic, Serial m)

instance Semigroup (NestedList a) where
  Nil              <> ys  = ys
  Cons x xs        <> ys  = Cons x (xs <> ys)
  xs               <> Nil = xs
  x                <> ys  = Cons x ys

instance Monoid (NestedList a) where
  mempty = Nil

infix 4 ===
(===) :: (Eq a, Show a) => a -> a -> Either Reason Reason
(===) a b | a == b    = Right $ "Matching: " ++ show a
          | otherwise = Left $ show a ++ " /= " ++ show b

prop_left_id :: (Monoid a, Show a, Eq a) => a -> Either Reason Reason
prop_left_id x = mempty <> x === x
prop_right_id :: (Monoid a, Show a, Eq a) => a -> Either Reason Reason
prop_right_id x = x <> mempty === x
prop_assoc :: (Monoid a, Show a, Eq a) => a -> a -> a -> Either Reason Reason
prop_assoc x y z = x <> (y <> z) === (x <> y) <> z

-- $> :set -XTypeApplications
-- $> smallCheck 3 (prop_left_id @(NestedList ()))
-- $> smallCheck 3 (prop_right_id @(NestedList ()))
-- $> smallCheck 3 (prop_assoc @(NestedList ()))

proptest = do
  smallCheck 3 (prop_left_id @(NestedList ()))
  smallCheck 3 (prop_right_id @(NestedList ()))
  smallCheck 3 (prop_assoc @(NestedList ()))

toNestedList :: [a] -> NestedList a
toNestedList [] = Nil
-- toNestedList [x] = Value x -- Should we have this rule?
toNestedList (x:xs) = Cons (Value x) (toNestedList xs)

takeNL 0 _ = []
takeNL n (Cons x y) = x : takeNL (n-1) y

takeTest = do
  print $ take 3 $ [1,2,3] <> undefined
  print $ takeNL 3 $ toNestedList [1,2,3] <> undefined
