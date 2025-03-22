{-# LANGUAGE RankNTypes #-}

-- | https://blog.jle.im/entry/sum-types-and-subtypes-and-unions.html
module Subtype where

x :: Num a => a
x = 1

y :: Fractional a => a
y = 1

f :: (forall a . Num a => a) -> Double
f x = x

f' :: (forall a . Fractional a => a) -> Double
f' x = x

g :: Fractional b => (forall a . Num a => a) -> b
g x = x

--- error: "Could not deduce (Fractional b) ..."
-- h :: Num b => (forall a . Fractional a => a) -> b
-- h x = x

gx :: Double
gx = g x
-- >>> gx
-- 1.0
