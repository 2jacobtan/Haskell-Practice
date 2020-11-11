{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Applicative (Applicative (liftA2))
import Data.Bifunctor (first)
-- import Control.Monad ((>=>))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance
  Functor f =>
  Functor (MaybeT f)
  where
  -- fmap f (MaybeT x)= MaybeT $ (fmap . fmap) f x -- "clever"
  fmap f (MaybeT x) = MaybeT $ fmap (fmap f) x -- clearer

instance
  Applicative f =>
  Applicative (MaybeT f)
  where
  pure = MaybeT . pure . pure
  MaybeT x <*> MaybeT y = MaybeT $ liftA2 (<*>) x y

instance
  Monad m =>
  Monad (MaybeT m)
  where
  return = pure
  MaybeT mma >>= f =
    MaybeT $
      mma >>= \case
        Nothing -> return Nothing
        Just a -> f' a
    where
      f' = runMaybeT . f

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance
  Functor f =>
  Functor (EitherT e f)
  where
  fmap f (EitherT x) = EitherT $ fmap (fmap f) x

instance
  Applicative f =>
  Applicative (EitherT e f)
  where
  pure = EitherT . pure . pure
  EitherT x <*> EitherT y = EitherT $ liftA2 (<*>) x y

instance
  Monad m =>
  Monad (EitherT e m)
  where
  return = pure
  EitherT mma >>= f =
    EitherT $
      mma >>= \case
        Left e -> return $ Left e
        Right a -> f' a
    where
      f' = runEitherT . f

swapEitherT ::
  (Functor f) =>
  EitherT e f a ->
  EitherT a f e
swapEitherT (EitherT x) =
  EitherT $ fmap swapEither x
  where
    swapEither :: Either e a -> Either a e
    swapEither = \case
      Left e -> Right e
      Right a -> Left a

eitherT ::
  Monad m =>
  (e -> m c) ->
  (a -> m c) ->
  EitherT e m a ->
  m c
eitherT f g (EitherT mma) =
  mma >>= \case
    Left e -> f e
    Right a -> g a

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance
  Functor f =>
  Functor (ReaderT r f)
  where
  fmap f (ReaderT x) = ReaderT $ fmap (fmap f) x

instance
  Applicative f =>
  Applicative (ReaderT r f)
  where
  pure = ReaderT . pure . pure
  ReaderT x <*> ReaderT y = ReaderT $ liftA2 (<*>) x y

instance
  Monad m =>
  Monad (ReaderT r m)
  where
  return = pure
  (>>=) ::
    ReaderT r m a ->
    (a -> ReaderT r m b) ->
    ReaderT r m b
  ReaderT mma >>= f =
    ReaderT $ \r ->
      mma r >>= \a -> f' a r
    where
      f' = runReaderT . f

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance
  Functor f =>
  Functor (StateT s f)
  where
  fmap f (StateT x) =
    StateT $ \s ->
      fmap (first f) (x s)

instance
  Monad m =>
  Applicative (StateT s m)
  where
  pure a =
    StateT $ \s ->
      return (a, s)
  
  -- | original solution
  -- StateT x <*> StateT y =
  --   StateT $ \s ->
  --     x s >>= \(x',s') ->
  --       y s' >>= \(y',s'') ->
  --         return (x' y',s'')
  
  -- | hlint suggestion
  -- StateT x <*> StateT y =
  --   StateT $ x >=> \(x',s') ->
  --       y s' >>= \(y',s'') ->
  --         return (x' y',s'')

  -- | do notation
  StateT x <*> StateT y =
   StateT $ \s -> do
     (x',s') <- x s
     (y',s'') <- y s'
     return (x' y',s'')

instance
  Monad m =>
  Monad (StateT s m)
  where
  return = pure
  StateT x >>= f =
    StateT $ \s -> do
      (x', s') <- x s
      f' x' s'
    where
      f' = runStateT . f

