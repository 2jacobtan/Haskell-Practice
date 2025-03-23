{-# LANGUAGE RankNTypes #-}

-- | https://blog.jle.im/entry/sum-types-and-subtypes-and-unions.html
module Subtype where

x :: forall a . Num a => a
x = 1

y :: forall a . Fractional a => a
y = 1.2

f :: (forall a . Num a => a) -> Double
f x = x

f' :: (forall a . Fractional a => a) -> Double
f' x = x

g :: (forall a . Num a => a) -> (forall b . Fractional b => b)
g x = x

--- error: "Could not deduce (Fractional b) ..."
-- h :: (forall a . Fractional a => a) -> (forall b . Num b => b)
-- h x = x

gx :: Double
gx = g x
-- >>> gx
-- 1.0
