{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Applicative (Applicative (liftA2))

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
