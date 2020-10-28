{-# LANGUAGE RankNTypes #-}
-- https://www.seas.upenn.edu/~cis194/fall16/lectures/14-lenses.html

-- data Lens a b = LensCons {
--     view :: a -> b,
--     overF :: forall t. Functor t => (b -> t b) -> (a -> t a)
--   }

import Data.Functor.Const
newtype I x = MkI x

unI :: I x -> x
unI (MkI x) = x

instance Functor I where
  fmap f x = MkI (f (unI x))

instance Applicative I where
  pure x = MkI x
  f <*> x = MkI $ (unI f) (unI x)


newtype C b x = MkC b
unC :: C b x -> b
unC (MkC b) = b

instance Functor (C b_) where
  fmap _ (MkC b) = MkC b

type Lens a b = forall t . Functor t => (b -> t b) -> (a -> t a)
type Traversal a b = forall t . Applicative t => (b -> t b) -> (a -> t a)

lensToTraversal :: Lens a b -> Traversal a b
lensToTraversal l = l

over :: Traversal a b -> (b -> b) -> (a -> a)
over l f a = unI $ l f' a
  where f' b = MkI $ f b

set :: Traversal a b -> b -> a -> a
set l x = over l (const x)

view :: Lens a b -> a -> b
view l a = unC $ l MkC a

this :: Traversal (Maybe a) a
this _ Nothing  = pure Nothing
this f (Just x) = Just <$> f x

elems :: Traversal [a] a
elems _ []     = pure []
elems f (x:xs) = (:) <$> f x <*> elems f xs


newtype CL b x = MkCL [b]

unCL :: CL b x -> [b]
unCL (MkCL bs) = bs

instance Functor (CL b_) where
  fmap _ (MkCL b) = MkCL b

instance Applicative (CL b_) where
  pure _ = MkCL []
  (MkCL bs1) <*> (MkCL bs2) = MkCL (bs1 ++ bs2)

listOf :: Traversal a b -> a -> [b]
listOf l a = unCL $ l (MkCL . (:[])) a

listOf' :: Traversal a b -> a -> [b]
listOf' l a = getConst $ l (Const . (:[])) a

