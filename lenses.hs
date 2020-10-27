{-# LANGUAGE RankNTypes #-}
-- https://www.seas.upenn.edu/~cis194/fall16/lectures/14-lenses.html

data Lens a b = LensCons {
    view :: a -> b,
    overF :: forall t. Functor t => (b -> t b) -> (a -> t a)
  }

newtype C b x = MkC b
unC :: C b x -> b
unC (MkC b) = b

instance Functor (C b_) where
  fmap _ (MkC b) = MkC b
