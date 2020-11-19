-- apdated from:
-- https://hackage.haskell.org/package/dlist-1.0/docs/src/Data.DList.Internal.html#DList

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Criterion.Main
newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = ($[]) . unDL
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL $ (x:) . unDL xs
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL $ unDL xs . (x:)
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs ys = DL $ unDL xs . unDL ys
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go = \case
      0 -> id
      n -> \xs -> go (n-1) ([n]++xs)

constructedDlist :: Int -> [Int]
constructedDlist i = toList $ go i empty
  where
    go = \case
      0 -> id
      n -> \xs -> go (n-1) (singleton n `append` xs)

main :: IO ()
main =
  defaultMain
    [ bench "concat list" $
        whnf schlemiel 123456,
      bench "concat dlist" $
        whnf constructedDlist 123456
    ]

