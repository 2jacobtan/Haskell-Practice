{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

type family OType f where
  OType (i -> x, i -> y) = i -> OType (x,y)
  OType (x,y) = (x,y)

-- | Apply same arguments to two functions with different output types
--
-- requires UndecidableInstances in Haskell
class ApplyTup f o where
  (%) :: f -> o
instance (OType f ~ o, ApplyTup' f o) => ApplyTup f o where
  (%) = applyTup'
class ApplyTup' f o where
  applyTup' :: f -> o
instance (ApplyTup' (f,g) o) => ApplyTup' (i->f,i->g) (i->o) where
  applyTup' (f,g) x = applyTup' (f x, g x)
instance ApplyTup' (f,g) (f,g) where
  applyTup' = id

f x y z = [x+y+z :: Int]
g x y z = Just (x+y+z :: Int)
g' x y z z' = Just (x+y+z+z' :: Int)

-- | type inference works, with currying
-- rez :: Int -> ([Int], Maybe Int)
rez = (%) (f,g) 1 2
-- >>> rez 3
-- ([6],Just 6)

rez' = (%) (f,g') 1 2
-- >>> (\(x,y)->(x,y 4)) $ rez' 3
-- ([6],Just 10)

--- | also works with 3 or more functions
rez'' = (%) (f, (%) (g,g'))
-- >>> (\(x,(y,z)) -> (x,y,z 4)) $ rez'' 1 2 3
-- ([6],Just 6,Just 10)
