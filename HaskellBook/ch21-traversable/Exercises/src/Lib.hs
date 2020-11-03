{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

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
  arbitrary = genericArbitrary (1 % 3 % ())
  -- arbitrary = frequency
  --   [
  --     (1, return Nil),
  --     (3, Cons <$> arbitrary <*> arbitrary)
  --   ]
instance EqProp a => EqProp (List a)


newtype Identity a = Identity a
  deriving (
    -- Eq, Ord,
    Show, Generic)
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


newtype Constant a b = Constant { getConstant :: a }
  deriving (Show, Generic)
instance Traversable (Constant a) where
  sequenceA (Constant x) = pure $ Constant x
instance Foldable (Constant a) where
  foldMap _ _ = mempty
instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x
instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = genericArbitraryU
instance (EqProp a, EqProp b) => EqProp (Constant a b)


data Optional a = Nada | Yep a
  deriving (Show, Generic)
instance Traversable Optional where
  sequenceA Nada = pure Nada
  sequenceA (Yep a) = Yep <$> a
instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a
instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a
instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = genericArbitraryU
instance EqProp a => EqProp (Optional a)


data Three a b c = Three a b c
  deriving (Show, Generic)
instance Traversable (Three a b) where
  sequenceA (Three x y z) = Three x y <$> z
instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)
instance Arbitrary c => Arbitrary (Three Int Int c) where
  arbitrary = genericArbitraryU
instance EqProp c => EqProp (Three Int Int c)


data Pair a b = Pair a b
  deriving (Show, Generic)
instance Traversable (Pair a) where
  sequenceA (Pair y z) = Pair y <$> z
instance Foldable (Pair a) where
  foldMap f (Pair _ z) = f z
instance Functor (Pair a) where
  fmap f (Pair y z) = Pair y (f z)
instance Arbitrary c => Arbitrary (Pair Int c) where
  arbitrary = genericArbitraryU
instance EqProp c => EqProp (Pair Int c)


data Big a b = Big a b b
  deriving (Show, Generic)
instance Traversable (Big a) where
  sequenceA (Big x y1 y2) = Big x <$> y1 <*> y2
instance Foldable (Big a) where
  foldMap f (Big _ y1 y2) = f y1 <> f y2
instance Functor (Big a) where
  fmap f (Big x y1 y2) = Big x (f y1) (f y2)
instance Arbitrary b => Arbitrary (Big Int b) where
  arbitrary = genericArbitraryU
instance EqProp b => EqProp (Big Int b)


data S n a = S (n a) a deriving (Eq, Show, Generic)
instance ( Functor n
  , Arbitrary (n a)
  , Arbitrary a )
  => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary
instance ( Applicative n
  , Testable (n Property)
  , Eq a
  , Eq (n a)
  , EqProp a)
  => EqProp (S n a) where
  (=-=) = eq
instance Traversable n
  => Traversable (S n) where
  traverse f (S nx x) = S <$> (traverse f nx) <*> (f x)
instance Foldable n => Foldable (S n) where
  foldMap f (S nx x) = foldMap f nx <> f x
instance Functor n => Functor (S n) where
  fmap f (S nx x) = S (fmap f nx) (f x)


data Tree a =
  Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show, Generic)
instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node tx y tz) = Node (fmap f tx) (f y) (fmap f tz)
-- foldMap is a bit easier
-- and looks more natural,
-- but you can do foldr, too,
-- for extra credit.
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node tx y tz) = foldMap f tx <> f y <> foldMap f tz
instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node tx y tz) = Node <$> traverse f tx <*> f y <*> traverse f tz
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genericArbitraryU
instance EqProp a => EqProp (Tree a)
