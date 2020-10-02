module NestedList where

data NestedList a = Nil
  | Value a
  | Cons (NestedList a) (NestedList a)
  deriving (Show, Eq)

instance Semigroup (NestedList a) where
  Nil              <> ys  = ys
  Cons x xs        <> ys  = Cons x (xs <> ys)
  xs               <> Nil = xs -- comment this out to see QuickCheck test fail
  x                <> ys  = Cons x ys

instance Monoid (NestedList a) where
  mempty = Nil

toNestedList :: [a] -> NestedList a
toNestedList [] = Nil
-- toNestedList [x] = Value x -- Should we have this rule?
toNestedList (x:xs) = Cons (Value x) (toNestedList xs)

takeNL _ Nil = []
takeNL 1 (Cons x _) = [x]
takeNL n (Cons x xs) = x : takeNL (n-1) xs

takeTest = do
  print $ take 3 $ [1,2,3] <> undefined
  print $ takeNL 3 $ toNestedList [1,2,3] <> undefined
