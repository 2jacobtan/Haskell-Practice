{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module TypeclassMetaprogramming where

import Numeric.Natural (Natural)

-- Alexis' version
type family ElementOf a where
  ElementOf [[a]] = ElementOf [a]
  ElementOf [a] = a

class Flatten a where
  flatten :: a -> [ElementOf a]

instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  flatten = flatten . concat

instance ElementOf [a] ~ a => Flatten [a] where
  flatten = id

-- my version
type family Flat a where
  Flat [[a]] = Flat [a]
  Flat [a] = [a]

class Flatten' a where
  flatten' :: a -> Flat a

instance {-# OVERLAPPING #-} Flatten' [a] => Flatten' [[a]] where
  flatten' = flatten' . concat

instance Flat [a] ~ [a] => Flatten' [a] where
  flatten' x = x

main :: IO ()
main = do
  let x = [[1 :: Int],[2]]
  print $ flatten x
  print $ flatten' x
  let y = [3 :: Int, 4]
  print $ flatten y
  print $ flatten' y
  let z = [[[5 :: Int]],[[6]]]
  print $ flatten z
  print $ flatten' z


-- __Generics

class GNumFields a where
  gnumFields :: a -> Natural

instance GNumFields (Leaf a) where
  gnumFields _ = 1

instance (GNumFields a, GNumFields b) => GNumFields (a,b) where
  gnumFields (a,b) = gnumFields a + gnumFields b

instance (GNumFields a, GNumFields b) => GNumFields (Either a b) where
  gnumFields (Left a)  = gnumFields a
  gnumFields (Right b) = gnumFields b

genericizeAuthentication :: Authentication -> Either (Username, Password) PublicKey
genericizeAuthentication (AuthBasic user pass) = Left (user, pass)
genericizeAuthentication (AuthSSH key)         = Right key

data Authentication
  = AuthBasic Username Password
  | AuthSSH PublicKey
type Username = String
type Password = String
type PublicKey = String

class Generic a where
  type Rep a
  genericize :: a -> Rep a

instance Generic Authentication where
  type Rep Authentication = Either (Leaf Username, Leaf Password) (Leaf PublicKey)
  genericize (AuthBasic user pass) = Left (Leaf user, Leaf pass)
  genericize (AuthSSH key)         = Right (Leaf key)

numFields :: (Generic a, GNumFields (Rep a)) => a -> Natural
numFields = gnumFields . genericize

newtype Leaf a = Leaf { getLeaf :: a }

data Foo = A (Either Int String) | B (Char, Bool)

instance Generic Foo where
  type Rep Foo = Either (Leaf (Either Int String)) (Leaf (Char, Bool))
  genericize (A x) = Left (Leaf x)
  genericize (B x) = Right (Leaf x)

instance GNumFields () where
  gnumFields _ = 0

instance Generic Bool where
  type Rep Bool = Either () ()
  genericize = \case
    False -> Left ()
    True -> Right ()

main2 = do
  print $ numFields $ AuthBasic "alyssa" "pass1234"
  print $ numFields $ AuthSSH "<key>"
  print $ numFields $ B ('a',True)
  print $ numFields True
  print $ numFields False