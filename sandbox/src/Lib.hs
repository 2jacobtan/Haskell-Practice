{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    , x, y) where
import Data.Char (intToDigit)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | right fold [Int] alongside left fold String
x :: ([Int],String)
x = foldr f ([],) [1..4] ""
  where 
    f :: Int -> (String -> ([Int],String)) -> String -> ([Int],String)
    f l r acc =
      let (r',a') = r (intToDigit l : acc)
      in (l : r', a')  -- (right fold result, left fold result)

-- | rank n types

fa :: a -> a
fa = id

fe :: (forall a . a -> a) -> b -> (Int, b)
fe fa' b = (fa' (1 :: Int), b)

y :: (Int, Bool)
y = fe fa True
-- >>> fe id True
-- (1,True)

-- ----------------------


-------------------------------------------------------------------------------
-- Mapping with ad-hoc polymorphic intermediate types
-------------------------------------------------------------------------------

data Selection = AnInt | ABool

class To (s :: Selection) a | s -> a where
  to :: a
instance To AnInt Int where to = 1 :: Int
instance To ABool Bool where to = True

anInt = to @AnInt
-- >>> anInt
-- 1

aBool = to @ABool
-- >>> aBool
-- True

anyInput :: a -> ()
anyInput = const ()

pipeline :: (forall a . Show a => a -> b) -> [b]
pipeline f = [f (to @AnInt), f (to @ABool) ]
p1 = pipeline anyInput
-- >>> p1
-- [(),()]

data ToBox = forall a . (Show a) => ToBox a 
deriving instance Show ToBox
hetList = [ToBox (to @AnInt), ToBox (to @ABool)]
-- >>> hetList
-- [ToBox 1,ToBox True]

p2 :: [ToBox]
p2 = pipeline ToBox
-- >>> pipeline ToBox
-- [ToBox 1,ToBox True]

data MyFunctor f a = MyFunctor (() -> a) (f a)
  deriving Functor
instance (Show a, Show (f a)) => Show (MyFunctor f a) where
  show (MyFunctor x y) = "MyFunctor (" ++ show (x ()) ++ ") (" ++ show y ++ ")"

-- csvX = MyFunctor (const 2) [3]
csvX = MyFunctor (const 2) (Just 3)
csvY = (*5) <$> csvX

-- >>> csvY
-- MyFunctor (10) (Just 15)

f x y = [x+y :: Int]
g x y = Just (x + y :: Int)

f' x y z = [x+y+z :: Int]
g' x y z = Just (x + y + z :: Int)

class Apply f i o | f -> i o where
  (%) :: f -> i -> o
instance Apply (i->o,i->p) i (o,p) where
  (%) :: (i -> o, i -> p) -> i -> (o, p)
  (%) (f,g) x = (f x, g x)

-- res :: (Int -> [Int], Int -> Maybe Int)
res = (f,g) % 1 -- :: (Int -> [Int], Int -> Maybe Int)

res1 = (f,g) % 1 % 2

res1' =
  let go fg = fg (1 :: Int) (2 :: Int)
  in (go f, go g)

{-
class Apply2 f o | o -> f where
  (%%) :: f -> o
-- | requires UndecidableInstances
instance (Apply2 (o1,o2) o) => Apply2 (i->o1,i->o2) (i -> o) where
  (%%) (f,g) x = (%%) (f x, g x)
instance Apply2 (o1,o2) (o1,o2) where
  (%%) = id

res2 :: ([Int], Maybe Int)
res2 = (%%) (f,g) 1 2

-- >>> res2
-- ([3],Just 3)

res2' :: ([Int], Maybe Int)
res2' = (%%) (f',g') 1 2 3

(rez2x, rez2y) = res2'
-}

class Apply3 f o where
  (%%%) :: f -> o
instance (Apply3 (o1,o2) o, i~j) => Apply3 (i->o1,i->o2) (j -> o) where
  (%%%) (f,g) x = (%%%) (f x, g x)
instance ((o1,o2)~(p1,p2)) => Apply3 (o1,o2) (p1,p2) where
  (%%%) = id
-- instance (o ~ (o1,o2)) => Apply3 o (() -> (o1,o2)) where
--   (%%%) = const

rez3 :: ([Int], Maybe Int)
rez3 = (%%%) (f',g') 1 2 3
rez3' = id @(_,_) $ (%%%) (f',g') 1 2 3
-- >>> rez3'
-- ([6],Just 6)
-- rez3'' = (%%%) (f',g') 1 2 3


--------------------------------------------------------------------------------
-- Advanced Overlapp
-- https://wiki.haskell.org/GHC/AdvancedOverlap
-- https://stackoverflow.com/questions/74479429/selecting-a-type-class-instance-based-on-the-context-once-again
--------------------------------------------------------------------------------

type family OType f o where
  OType (i -> x, i -> y) o = o
  OType (x,y) _ = (x,y)
  -- OType x _ = x

class Apply4 f o where
  (%-%) :: f -> o
-- | requires UndecidableInstances
instance (OType f o ~ p, Apply4' p f o) => Apply4 f o where
  (%-%) = (%%%%) (undefined :: p)
class Apply4' p f o where
  (%%%%) :: p -> f -> o
instance (Apply4 (o1,o2) o, i~j) => Apply4' _a (i->o1,i->o2) (j -> o) where
  (%%%%) _ (f,g) x = (%-%) (f x, g x)
instance ((o1,o2)~q) => Apply4' (p1,p2) (o1,o2) q where
  (%%%%) _ = id

rez4 :: ([Int], Maybe Int)
rez4 = (%-%) (f',g') 1 2 3
rez4' = id @(_,_) $ (%-%) (f',g') 1 2 3
-- >>> rez4'
-- ([6],Just 6)

rez4'' = (%-%) (f',g') 1 2 3
-- >>> rez4''
-- ([6],Just 6)

rez4''' :: Int -> ([Int], Maybe Int)
rez4''' = (%-%) (f',g') 1 2
-- >>> rez4''' 3
-- ([6],Just 6)


type family OType5 f where
  OType5 (i -> x, i -> y) = i -> OType5 (x,y)
  OType5 (x,y) = (x,y)
  -- OType5 x = x

class Apply5 f o where
  (%--%) :: f -> o
-- | requires UndecidableInstances
instance (OType5 f ~ p, Apply5' p f o) => Apply5 f o where
  (%--%) = (%%%%%) (undefined :: p)
class Apply5' p f o where
  (%%%%%) :: p -> f -> o
instance (Apply5' o (o1,o2) o, i~j, p~(j->o)) => Apply5' p (i->o1,i->o2) (j->o) where
  (%%%%%) _ (f,g) x = (%%%%%) (undefined :: o) (f x, g x)
instance ((o1,o2)~q) => Apply5' (p1,p2) (o1,o2) q where
  (%%%%%) _ = id

rez5 = (%--%) (f',g') 1 2
-- >>> rez5 3
-- ([6],Just 6)

{-
NOTE:  ApplyTup (final form of Apply5) is available in AdvancedOverlap.hs
-}


--- | associated data family

class Attached a where
  type Attach a
  data WithAttach a
  getAttach :: WithAttach a -> Attach a

instance Attached Bool where
  type Attach Bool = Int
  data WithAttach Bool = WithAttachBool Int Bool
  getAttach (WithAttachBool x _) = x

att = getAttach (WithAttachBool 1 True)

-- >>> att
-- 1
