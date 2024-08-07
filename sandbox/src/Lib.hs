{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DatatypeContexts #-}

module Lib
    ( someFunc
    , x, y) where
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

-- | rank n types

fa :: a -> a
fa = id

fe :: (forall a . a -> a) -> b -> (Int, b)
fe fa' b = (fa' (1 :: Int), b)

y :: (Int, Bool)
y = fe fa True
-- >>> fe id True
-- (1,True)

-- ----------------------


-------------------------------------------------------------------------------
-- Mapping with ad-hoc polymorphic intermediate types
-------------------------------------------------------------------------------

data Selection = AnInt | ABool

class To (s :: Selection) a | s -> a where
  to :: a
instance To AnInt Int where to = 1 :: Int
instance To ABool Bool where to = True

anInt = to @AnInt
-- >>> anInt
-- 1

aBool = to @ABool
-- >>> aBool
-- True

anyInput :: a -> ()
anyInput = const ()

pipeline :: (forall a . Show a => a -> b) -> [b]
pipeline f = [f (to @AnInt), f (to @ABool) ]
p1 = pipeline anyInput
-- >>> p1
-- [(),()]

data ToBox = forall a . (Show a) => ToBox a 
deriving instance Show ToBox
hetList = [ToBox (to @AnInt), ToBox (to @ABool)]
-- >>> hetList
-- [ToBox 1,ToBox True]

p2 :: [ToBox]
p2 = pipeline ToBox
-- >>> pipeline ToBox
-- [ToBox 1,ToBox True]

data CSV f a = CSV (() -> a) (f a)
  deriving Functor
instance (Show a, Show (f a)) => Show (CSV f a) where
  show (CSV x y) = "CSV (" ++ show (x ()) ++ ") (" ++ show y ++ ")"

newtype One a = One a

-- csvX = CSV (const 2) [3]
csvX = CSV (const 2) (Just 3)
csvY = (*5) <$> csvX

-- >>> csvY
-- CSV (10) (Just 15)
