{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | https://blog.jle.im/entry/sum-types-and-subtypes-and-unions.html
module Subtype where

import Data.Kind (Constraint)

x :: forall a . Num a => a
x = 1

y :: forall a . Fractional a => a
y = 1.2

type IsDouble a = (a ~ Double) :: Constraint

-- | `Num` can substitute for `IsDouble`, since `Num` is a weaker precondition.
f :: (forall a . Num a => a) -> (forall b . IsDouble b => b)
f x = x

f' :: (forall a . Fractional a => a) -> (forall b . IsDouble b => b)
f' x = x

-- | `Num` can substitute for `Fractional`, since `Num` is a weaker precondition.
g :: (forall a . Num a => a) -> (forall b . Fractional b => b)
g x = x

--- error: "Could not deduce (Fractional b) ..."
-- | If I can produce any Fractional, then I can produce any Num.
-- h :: (forall a . Fractional a => a) -> (forall b . Num b => b)
-- h x = x

-- | Liskov substitution: given a program `p` that takes any Num, 
-- | you can also give it any Fractional.
-- | I.e. Fractional is a subtype of Num, since Fractional can be substituted for Num.
j :: (forall a . Num a        => a -> Int)
  -> (forall b . Fractional b => b -> Int)
j p = p
