{-# LANGUAGE TypeApplications #-}

module Main where

import Lib
import NestedList
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1+1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

    it "left identity" $ do
      property $ prop_left_id @(NestedList ())
    it "right identity" $ do
      property $ prop_right_id @(NestedList ())
    it "associativity" $ do
      property $ prop_assoc @(NestedList ())


instance Arbitrary a => Arbitrary (NestedList a) where
  arbitrary = arbitrary >>= \x -> return $ Value x


prop_left_id :: (Monoid a, Show a, Eq a) => a -> Bool
prop_left_id x = mempty <> x == x

prop_right_id :: (Monoid a, Show a, Eq a) => a -> Bool
prop_right_id x = x <> mempty == x

prop_assoc :: (Monoid a, Show a, Eq a) => a -> a -> a -> Bool
prop_assoc x y z = x <> (y <> z) == (x <> y) <> z

