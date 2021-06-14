{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Set (Set)
import qualified Data.Set as Set

-- https://twitter.com/chris__martin/status/1401242486702428164

traverseSet :: (Ord a1, Applicative f) =>
  (a2 -> f a1) -> Set a2 -> f (Set a1)
traverseSet f = fmap Set.fromList . traverse f . Set.toList

traverseSet' :: (Foldable t, Monoid (f (Set a1)), Functor f) =>
  (a2 -> f a1) -> t a2 -> f (Set a1)
traverseSet' f = foldMap (fmap Set.singleton . f)

-- (Applicative f, Monoid a) => Monoid (Ap f a)
--- ^ This instance exists in:
--    https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Monoid.html#t:Monoid

-- By abave pattern, it looks like
-- instance (Applicative f, Monoid a) => Monoid (f a)
-- is defined for every applicative f.

-- my version
-- kinda bad because of Ord (f a1) constraint
traverseSet'' :: (Applicative f, Ord a1, Ord (f a1)) =>
  (a2 -> f a1) -> Set a2 -> f (Set a1)
traverseSet'' f = foldr ((<*>) . fmap Set.insert) (pure mempty) . Set.map f

--- | Even though `Ord a1` is not an explicit constraint in type signature of  traverseSet', it's still required. Cuz (<>) = union, which requires Ord.
-- x = traverseSet' (pure @Maybe . pure @IO) (Set.fromList [1..3 :: Int])

zz :: Set (IO Bool)
zz = Set.singleton . pure @IO $ True

-- zzz :: Set (IO Bool)
-- zzz = zz <> undefined  

data Null deriving Show

yy :: Set Null
yy = Set.empty @Null

-- yyy :: Set Null
-- yyy = yy <> undefined



----------------------------------------------------------------



-- 2021-06-14

foo :: forall a . (Show a, Num a) => a -> Int
foo _ = length . show $ (1 :: (Show a, Num a) => a)

bar :: String
bar = if foo (0.0 :: Double) == 3
  then "-XScopedTypeVariables"
  else "-XNoScopedTypeVariables"

---

-- bazz :: a -> a
-- bazz x = fizz
--   where
--     fizz :: forall . c
--     fizz = x

-- Cf. https://www.youtube.com/watch?v=ijicXvkipFI&t=9m48s
--    "forall or nothing" rule
