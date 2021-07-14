{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module WitchTest where

import Witch ( From (from), into )
import Data.Set (Set)
import qualified Data.Set as Set

-- https://hackage.haskell.org/package/witch-0.3.4.0/docs/Witch.html

newtype Integ = Integ Int deriving (Eq, Ord)
instance From Integ Int
instance From Int Integ

newtype List a = List [a]
  deriving stock Show
  deriving newtype Functor
instance From (List a) [a]
instance From [a] (List a)

instance {-# OVERLAPPABLE #-}
  From [a] b  -- undecidable instance
  => From (List a) b where
  from = into . into @[a]
instance {-# OVERLAPPABLE #-}
  From b [a]  -- undecidable instance
  => From b (List a) where
  from = into . into @[a]

x :: Integer
x = from (42 :: Int)  -- Int to Integ to Integer

y :: List Bool
y = from [True]

z :: Set Integer
z = into @(Set _) @(List _) . fmap (into @Integer @Int) $ List [1,2,3 :: Int]

zz :: List Integer
zz = into @(List _) @(Set _) . Set.map (into @Integer @Int) $ Set.fromList [1,2,3 :: Int]

-- >>> x
-- >>> y
-- >>> z
-- 42
-- List [True]
-- fromList [1,2,3]
