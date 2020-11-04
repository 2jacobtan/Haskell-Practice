{-# LANGUAGE InstanceSigs #-}

import Data.Char (toUpper)
-- import Control.Monad.Trans.Reader
-- import Data.Functor.Identity (Identity(Identity))
import Control.Applicative (liftA2)

cap = map toUpper

rev = reverse

composed = cap . rev

fmapped = cap <$> rev

tupled1 = (,) <$> cap <*> rev
tupled2 = do
  a <- cap
  b <- rev
  return $ (,) a b
tupled3 = cap >>= \a -> rev >>= \b -> return $ (,) a b

test1 = tupled1 "Julie"
test2 = tupled2 "Julie"
test3 = tupled3 "Julie"


-- | Using real Reader
-- ask :: Reader a a
-- ask = ReaderT $ fmap Identity id

-- ask'' :: Reader a a
-- ask'' = ReaderT $ Identity . id

-- ask' :: Reader a a
-- ask' = reader id

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  -- fmap :: (a -> b)
  --   -> Reader r a
  --   -> Reader r b
  fmap f (Reader ra) =
    Reader $ \r -> f (ra r)

ask :: Reader a a
ask = Reader id

newtype HumanName = HumanName String
  deriving (Eq, Show)

newtype DogName = DogName String
  deriving (Eq, Show)

newtype Address = Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName,
    dogName :: DogName,
    address :: Address
  }
  deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName,
    dogsAddress :: Address
  }
  deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
  (DogName "Barkley")
  (Address "Sesame Street")

chris :: Person
chris =
  Person (HumanName "Chris Allen")
  (DogName "Papu")
  (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)
-- with Reader
getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' =
  liftA2 Dog dogName address

getDogR'' :: Person -> Dog
getDogR'' =
  myLiftA2 Dog dogName address


myLiftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
-- myLiftA2 = \x y -> (x <$> y <*>)
-- myLiftA2 = \x -> (<*>) . (x <$>)
myLiftA2 = ((<*>) . ) . (<$>)


asks :: (r -> a) -> Reader r a
asks = Reader

instance Applicative (Reader r) where
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)
  pure = Reader . const

getDogRM' :: Person -> Dog
getDogRM' = do
  name <- dogName
  addy <- address
  return $ Dog name addy

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
    -> (a -> Reader r b)
    -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r
  
getDogRM :: Reader Person Dog
getDogRM = do
  name <- asks dogName
  addy <- asks address
  return $ Dog name addy


withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader f m = Reader $ runReader m . f

