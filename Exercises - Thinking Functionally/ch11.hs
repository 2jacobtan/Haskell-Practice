{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data.Fixed (mod')
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

-- Exercise A

newtype Angle = Angle Double deriving Show

instance Eq Angle where
  Angle a == Angle b
    = a `mod'` (2*pi) == b `mod'` (2*pi)

newtype Angle2 = Angle2 Double deriving Show

instance Eq Angle2 where
  Angle2 a == Angle2 b
    = reduce a == reduce b
    where
    reduce x
      | x > r = reduce (x - r)
      | x < 0 = reduce (x + r)
      | otherwise = x
      where r = 2*pi


-- Exercise B

newtype Parser a = Parser (String -> Maybe (a,String))

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return x = Parser $ \s -> Just (x, s)
  Parser f >>= g
    = Parser $ \s -> case f s of
    Nothing -> Nothing
    Just (x, s') ->
      let Parser p = g x
      in p s'

-- Exercise C

