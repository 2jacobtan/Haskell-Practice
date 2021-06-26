-- https://www.youtube.com/watch?v=PHS3Q-tRjFQ&list=PLyzwHTVJlRc9QcF_tdqc9RdxJED8Mvyh1&index=26
-- @rae: How to program in types with length-indexed vectors: Part 1
-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

-- https://www.youtube.com/watch?v=PHS3Q-tRjFQ&list=PLyzwHTVJlRc9QcF_tdqc9RdxJED8Mvyh1&index=26
-- @rae: How to program in types with length-indexed vectors: Part 1
-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}
import Data.Kind (Type, Constraint)
import Prelude (Bool (..), Show, (&&), otherwise, ($), Eq ((==)), (||))

data Nat = Zero | Succ Nat

type Vec :: Nat -> Type -> Type
data Vec n a where
  Nil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a

infixr 5 :>

deriving stock instance Show a => Show (Vec n a)

and :: Vec n Bool -> Bool
and (b :> bs) = b && and bs
and Nil = True

head :: Vec (Succ n) a -> a
head (x :> _) = x

init :: Vec (Succ n) a -> Vec n a
init (x :> xs@(_ :> _)) = x :> init xs
init (_ :> Nil) = Nil

map :: (a -> b) -> Vec n a -> Vec n b
map _ Nil = Nil
map f (x :> xs) = f x :> map f xs

type SNat :: Nat -> Type
data SNat n where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

length :: Vec n a -> SNat n
length Nil = SZero
length (_ :> xs) = SSucc (length xs)

replicate :: SNat n -> a -> Vec n a
replicate SZero _ = Nil
replicate (SSucc n) x = x :> replicate n x

type (+) :: Nat -> Nat -> Nat
type family (+) n m where
  (+) Zero m = m
  (+) (Succ n) m = Succ (n + m)

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(++) Nil ys = ys
(++) (x :> xs) ys = x :> (++) xs ys

type Min :: Nat -> Nat -> Nat
type family Min n m where
  Min Zero _ = Zero
  Min _ Zero = Zero
  Min (Succ n) (Succ m) = Succ (Min n m)

take :: SNat n -> Vec m a -> Vec (Min n m) a
take SZero _ = Nil
take _ Nil = Nil
take (SSucc n) (x :> xs) = x :> take n xs

type Drop :: Nat -> Nat -> Nat
type family Drop n m where
  Drop Zero m = m
  Drop _ Zero = Zero
  Drop (Succ n) (Succ m) = Drop n m

drop :: SNat n -> Vec m a -> Vec (Drop n m) a
drop SZero xs = xs
drop _ Nil = Nil
drop (SSucc n) (_ :> xs) = drop n xs

type EVec :: (Nat -> Constraint) -> Type -> Type
data EVec c a where
  MkEVec :: c n => Vec n a -> EVec c a

type KnowNothing :: Nat -> Constraint
class KnowNothing n
instance KnowNothing n

type (>=) :: Nat -> Nat -> Constraint
class (>=) n m
instance n >= Zero
instance m >= n => Succ m >= Succ n
instance {-# OVERLAPPABLE #-} m >= n => Succ m >= n


filter :: (a -> Bool) -> Vec n a -> EVec ((>=) n) a
filter _ Nil = MkEVec Nil
filter p (x:>xs)
  | p x = case filter p xs of MkEVec tail ->  MkEVec $ x :> tail
  | otherwise = case filter p xs of MkEVec v -> MkEVec v

elem :: Eq a => a -> Vec n a -> Bool
elem _ Nil = False
elem x (y :> ys) = x == y || elem x ys

type (<=) :: Nat -> Nat -> Constraint
class (<=) n m
instance {-# incoherent #-} Zero <= m
instance {-# incoherent #-} n <= n
instance {-# incoherent #-} n <= m => n <= Succ m
-- instance {-# OVERLAPPABLE #-} n <= m => n <= Succ m

type (<&&>) :: (Nat -> Constraint) -> (Nat -> Constraint) -> Nat -> Constraint
class (cond1 n, cond2 n) => (<&&>) cond1 cond2 n
instance (cond1 n, cond2 n) => (cond1 <&&> cond2) n

-- union :: Eq a => Vec n a -> Vec m a -> EVec ((<=) n <&&> (<=) m) a
union :: Eq a => Vec n a -> Vec m a -> EVec ((<=) m) a
union Nil ys = MkEVec ys
union xs Nil = MkEVec xs
union (x :> xs) ys
  | x `elem` ys = case xs `union` ys of MkEVec v -> MkEVec v
  | otherwise = case xs `union` ys of MkEVec v -> MkEVec $ x :> v