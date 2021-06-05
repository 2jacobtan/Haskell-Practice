import Data.Set (Set, fromList, singleton, toList)

-- https://twitter.com/chris__martin/status/1401242486702428164

traverseSet' :: (Foldable t, Monoid (f (Set a1)), Functor f) =>
  (a2 -> f a1) -> t a2 -> f (Set a1)
traverseSet' f = foldMap (fmap singleton . f)

traverseSet :: (Ord a1, Applicative f) =>
  (a2 -> f a1) -> Set a2 -> f (Set a1)
traverseSet f = fmap fromList . traverse f . toList


-- (Applicative f, Monoid a) => Monoid (Ap f a)
--- ^ This instance exists in:
--    https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Monoid.html#t:Monoid

-- By abave pattern, it looks like
-- instance (Applicative f, Monoid a) => Monoid (f a)
-- is defined for every applicative f.

------------

