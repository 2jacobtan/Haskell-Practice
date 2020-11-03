{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Lib where

import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Generic.Random

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data List a =
  Nil | Cons a (List a) deriving (Show, Generic)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs
instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs
  -- | deliberately fail testing
  -- sequenceA (Cons x xs) = Cons <$> x <*> pure Nil
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genericArbitraryU
  -- arbitrary = frequency
  --   [
  --     (1, return Nil),
  --     (3, Cons <$> arbitrary <*> arbitrary)
  --   ]
instance EqProp a => EqProp (List a)

newtype Identity a = Identity a
  deriving (Eq, Ord, Show, Generic)
instance Traversable Identity where
  traverse f (Identity a) = fmap Identity (f a)
instance Foldable Identity where
  foldMap f (Identity a) = f a
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genericArbitraryU
  -- arbitrary = Identity <$> arbitrary
instance EqProp a => EqProp (Identity a)