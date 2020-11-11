{-# LANGUAGE LambdaCase #-}
module Lib
  ( someFunc
  ) where

import Control.Applicative (Applicative(liftA2))
someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}

instance
  Functor f =>
  Functor (MaybeT f)
  where
  -- fmap f (MaybeT x)= MaybeT $ (fmap . fmap) f x -- "clever"
  fmap f (MaybeT x)= MaybeT $ fmap (fmap f) x -- clearer

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
