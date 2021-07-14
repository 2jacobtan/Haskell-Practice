{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module WitchTest where

import Witch ( From (from), into, via )
import Data.Set (Set)
import qualified Data.Set as Set

-- https://hackage.haskell.org/package/witch-0.3.4.0/docs/Witch.html

newtype List a = List [a]
  deriving stock Show
  deriving newtype Functor
instance From (List a) [a]
instance From [a] (List a)

instance {-# OVERLAPPABLE #-}
  From [a] b  -- undecidable instance
  => From (List a) b where
  -- from = into . into @[a]
  from = via @[a]
instance {-# OVERLAPPABLE #-}
  From b [a]  -- undecidable instance
  => From b (List a) where
  -- from = into . into @[a]
  from = via @[a]

-- Taylor Fausak's response
-- alternative to above instances
-- https://www.reddit.com/r/haskell/comments/oje9k9/cast_haskell_values_with_witch/h55nmu2/?utm_source=reddit&utm_medium=web2x&context=3
-- instance Ord a => From (List a) (Set a) where
--   from = via @[a]
-- instance From (Set a) (List a) where
--   from = via @[a]

x :: List Bool
x = from [True]

y :: Set Integer
y = into @(Set _) @(List _) . fmap (into @Integer @Int) $ List [1,2,3 :: Int]

z :: List Integer
z = into @(List _) @(Set _) . Set.map (into @Integer @Int) $ Set.fromList [1,2,3 :: Int]

-- >>> x
-- >>> y
-- >>> z
-- List [True]
-- fromList [1,2,3]
-- List [1,2,3]
