{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

type family OType5 f where
  OType5 (i -> x, i -> y) = i -> OType5 (x,y)
  OType5 (x,y) = (x,y)
  -- OType5 x = x

-- | requires UndecidableInstances and ScopedTypeVariables in Haskell
class Apply5 f o where
  (%--%) :: f -> o
instance (OType5 f ~ p, Apply5' p f o) => Apply5 f o where
  (%--%) = (%%%%%) (undefined :: p)
class Apply5' p f o where
  (%%%%%) :: p -> f -> o
instance (Apply5' o (o1,o2) o, i~j, p~(j->o)) => Apply5' p (i->o1,i->o2) (j->o) where
  (%%%%%) _ (f,g) x = (%%%%%) (undefined :: o) (f x, g x)
instance ((o1,o2)~q) => Apply5' (p1,p2) (o1,o2) q where
  (%%%%%) _ = id

f' x y z = [x+y+z :: Int]
g' x y z = Just (x + y + z :: Int)

rez5 = (%--%) (f',g') 1 2
-- >>> rez5 3
-- ([6],Just 6)
