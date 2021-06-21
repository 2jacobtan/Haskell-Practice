-- https://lexi-lambda.github.io/blog/2021/03/25/an-introduction-to-typeclass-metaprogramming/

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
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

main2 :: IO ()
main2 = do
  print $ numFields $ AuthBasic "alyssa" "pass1234"
  print $ numFields $ AuthSSH "<key>"
  print $ numFields $ B ('a',True)
  print $ numFields True
  print $ numFields False


-- Dependent Types

-- data HList as where
--   HNil :: HList '[]
--   HCons :: Show a => a -> HList as -> HList (a ': as)
-- deriving instance Show (HList as)

data family HList (as :: [*])
data instance HList '[] = HNil deriving Show
data instance HList (a ': as) = HCons a (HList as)
deriving instance (Show a, Show (HList as)) => Show (HList (a ': as))

-- instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
--   show (HCons x xs) = "(" ++ show x ++ ") `HCons` (" ++ show xs ++ ")" 

infixr 5 `HCons`

data Even as where
  EvenNil  :: Even '[]
  EvenCons :: Even as -> Even (a ': b ': as)

type family PairUp as where
  PairUp '[] = '[]
  PairUp (a ': b ': as) = (a, b) ': PairUp as

class IsEven as where
  evenProof :: Even as

instance IsEven '[] where
  evenProof = EvenNil

instance IsEven as => IsEven (a ': b ': as) where
  evenProof = EvenCons evenProof

pairUp :: IsEven as => HList as -> HList (PairUp as)
pairUp = go evenProof where
  go :: Even as -> HList as -> HList (PairUp as)
  go EvenNil HNil = HNil
  go (EvenCons even_) (x `HCons` y `HCons` xs) = (x,y) `HCons` go even_ xs

--- | works with GADT
-- consH :: Show a => a -> HList as -> HList (a ': as)
-- consH x HNil = HCons x HNil
-- consH x (HCons y ys) = HCons x (HCons y ys)

--- | works with data family
class ConsH as where
  consH :: Show a => a -> HList as -> HList (a ': as)
instance ConsH '[] where
  consH x HNil = HCons x HNil
instance ConsH (a ': as) where
  consH x (HCons y ys) = HCons x (HCons y ys)

main3 :: IO ()
main3 = do
  print $ pairUp HNil
  print $ pairUp ((1 :: Int) `HCons` True `HCons` HNil)


-- Subtyping constraints

data GQLKind
  = Both
  | Input
  | Output

-- data GQLType k where
--   TScalar      :: GQLType 'Both
--   TInputObject :: InputObjectInfo -> GQLType 'Input
--   TIObject     :: ObjectInfo -> GQLType 'Output

data SubKind k1 k2 where
  KRefl :: SubKind k k
  KBoth :: SubKind 'Both k

class IsSubKind (k1 :: GQLKind) (k2 :: GQLKind) where
  -- subKindProof :: SubKind k1 k2

instance IsSubKind 'Both k where
  -- subKindProof = KBoth

instance (k ~ 'Input) => IsSubKind 'Input k where
  -- subKindProof = KRefl

instance (k ~ 'Output) => IsSubKind 'Output k where
  -- subKindProof = KRefl

data GQLParser (k :: GQLKind) a
nullable :: IsSubKind k 'Input => GQLParser k a -> GQLParser k (Maybe a)
nullable = undefined 

_ = nullable (undefined :: GQLParser 'Both a)
_ = nullable (undefined :: GQLParser 'Input a)
-- _ = nullable (undefined :: GQLParser 'Output a) -- type error