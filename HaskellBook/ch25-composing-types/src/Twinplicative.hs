{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (Applicative(liftA2))
newtype Compose f g a = Compose {getCompose :: f (g a)}
  deriving (Eq, Show)

instance
  (Functor f, Functor g) =>
  Functor (Compose f g)
  where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance
  (Applicative f, Applicative g) =>
  Applicative (Compose f g)
  where
  pure :: a -> Compose f g a
  -- pure a = Compose $ pure (pure  a)
  pure = Compose . pure . pure
  (<*>) ::
    Compose f g (a -> b) ->
    Compose f g a ->
    Compose f g b
  -- Compose x <*> Compose y = Compose $ fmap (<*>) x <*> y
  Compose x <*> Compose y = Compose $ liftA2 (<*>) x y

instance
  (Foldable f, Foldable g) =>
  Foldable (Compose f g)
  where
  -- foldMap f (Compose x) = (foldMap . foldMap) f x -- "clever"
  foldMap f (Compose x) = foldMap (foldMap f) x -- clearer

instance
  (Traversable s, Traversable t) =>
  Traversable (Compose s t)
  where
  -- s (t (f a)) -> f (s (t b))
  traverse f (Compose x) = fmap Compose $ traverse (traverse f) x


class BiFunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap ::
    (a -> b) ->
    (c -> d) ->
    p a c ->
    p b d
  bimap f g = first f . second g
  
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  
  second :: (c -> d) -> p a c -> p a d
  second f = bimap id f

data Deux a b = Deux a b
instance BiFunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a
instance BiFunctor Const where
  bimap f _ (Const a) = Const $ f a

data Drei z a b = Drei z a b
instance BiFunctor (Drei z) where
  bimap f g (Drei z a b) = Drei z (f a) (g b)

data SuperDrei z a b = SuperDrei z a
instance BiFunctor (SuperDrei z) where
  bimap f _ (SuperDrei z a) = SuperDrei z (f a)

data SemiDrei z a b = SemiDrei z
instance BiFunctor (SemiDrei z) where
  bimap _ _ (SemiDrei z) = SemiDrei z

data Quadriceps y z a b = Quadzzz y z a b
instance BiFunctor (Quadriceps y z) where
  bimap f g (Quadzzz y z a b) = Quadzzz y z (f a) (g b)

data Either a b = Left a | Right b
instance BiFunctor (Main.Either) where
  bimap f g = \case
    Main.Left a -> Main.Left $ f a
    Main.Right b -> Main.Right $ g b


newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a}
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance
  (Functor m) =>
  Functor (IdentityT m)
  where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance
  Applicative m =>
  Applicative (IdentityT m)
  where
  pure = IdentityT . pure
  IdentityT fab <*> IdentityT fa = IdentityT $ fab <*> fa

instance Monad Identity where
  return = pure

  (>>=) (Identity a) = ($ a) -- "clever"
  -- Identity a >>= f = f a -- clearer

instance
  Monad m =>
  Monad (IdentityT m) where
  return = pure

  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

  