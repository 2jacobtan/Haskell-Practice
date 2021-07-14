{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeApplications #-}
module WitchTest where

import Witch ( From (from) )

-- https://hackage.haskell.org/package/witch-0.3.4.0/docs/Witch.html#g:2
newtype Name = Name String
instance From Name String
instance From String Name

newtype List a = List [a]
instance From (List a) [a]
instance From [a] (List a)

newtype Integ = Integ Int
instance From Integ Int
instance From Int Integ

x :: Integer
x = from (2 :: Int)  -- Int to Integ to Integer