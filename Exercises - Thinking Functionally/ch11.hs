{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data.Fixed (mod')
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (MonadPlus, liftM, ap)

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
fail = \s -> []
fail >> p = \s0 ->
  case (\s -> []) s0 of
    [] -> []
    -- ...

-- hence: fail >> p = \s0 -> [] = fail


-- Exercise D

-- if both p and q would parse successfully, then
-- (p <|> q)
-- = Parser $ \s -> parse p s ++ parse q s
-- = Parser $ \s -> [(a,s)] ++ [(b,t)]
-- = Parser $ \s -> [(a,s),(b,t)]

-- limit (Parser pq) = Parser $ \s -> case pq s of
--   [] -> []
--   x -> head x


-- Exercise E

-- instance MonadPlus Parser where
--   mzero = Parser $ \s -> [] -- = fail
--   p `mplus` q = Parser $ \s -> case apply p s of
--     Nothing -> apply q s
--     Just x -> x


-- Execrise F

-- if p is Just x, and Just x >>= f is Nothing, but q >>= f is Just y


-- Exercise G









