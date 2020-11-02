{-# LANGUAGE DeriveGeneric #-}

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
  arbitrary = genericArbitrary uniform
instance EqProp a => EqProp (List a)