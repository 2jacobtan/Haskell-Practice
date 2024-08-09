{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

type family OType6 f where
  OType6 (i -> x, i -> y) = i -> OType6 (x,y)
  OType6 (x,y) = (x,y)

-- | requires UndecidableInstances and ScopedTypeVariables in Haskell
class Apply6 f o where
  (%--%) :: f -> o
instance (OType6 f ~ o, Apply6' f o) => Apply6 f o where
  (%--%) = (%%%%%)
class Apply6' f o where
  (%%%%%) :: f -> o
instance (Apply6' (o1,o2) o, i~j) => Apply6' (i->o1,i->o2) (j->o) where
  (%%%%%) (f,g) x = (%%%%%) (f x, g x)
instance Apply6' (o1,o2) (o1,o2) where
  (%%%%%) = id

f x y z = [x+y+z :: Int]
g x y z = Just (x+y+z :: Int)
g' x y z z' = Just (x+y+z+z' :: Int)

rez6 = (%--%) (f,g) 1 2
-- >>> rez6 3
-- ([6],Just 6)

rez6' = (%--%) (f,g') 1 2
-- >>> (\(x,y)->(x,y 4)) $ rez6' 3
-- ([6],Just 10)
