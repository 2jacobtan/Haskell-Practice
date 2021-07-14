{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
-- {-# OPTIONS_GHC -Wno-orphans#-}
{-# LANGUAGE UndecidableInstances #-}
module WitchTest where

import Witch ( From (from), into )
import Data.Set (Set)

-- https://hackage.haskell.org/package/witch-0.3.4.0/docs/Witch.html

newtype List a = List [a] deriving newtype Functor deriving Show
instance From (List a) [a]
instance From [a] (List a)
instance {-# OVERLAPPABLE #-} From [a] b => From (List a) b where
  from = into . into @[a]

newtype Integ = Integ Int deriving (Eq, Ord)
instance From Integ Int
instance From Int Integ

x :: Integer
x = from (42 :: Int)  -- Int to Integ to Integer

y :: List Bool
y = from [True]

z :: Set Integer
z = into . fmap (into @Integer) $ List [1,2,3 :: Int]

-- >>> x
-- >>> y
-- >>> z
-- 42
-- List [True]
-- fromList [1,2,3]
