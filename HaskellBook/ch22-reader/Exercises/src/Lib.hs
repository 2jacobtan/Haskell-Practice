module Lib where

import Control.Applicative
-- import Data.Maybe
x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup l = foldr f Nothing
  where
    f (a,b) xs
      | a == l = Just b
      | otherwise = xs

xs = Lib.lookup 3 $ zip x y

ys = Lib.lookup 6 $ zip y z

zs = Lib.lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = Lib.lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 = liftA2 (,) z' z'

uncurry f (a,b) = f a b

summed = Lib.uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

fromMaybe :: a -> Maybe a -> a
fromMaybe a m = maybe a id m

sequA m = sequenceA [(>3), (<8), even] m
s' = summed <$> ((,) <$> xs <*> ys)

