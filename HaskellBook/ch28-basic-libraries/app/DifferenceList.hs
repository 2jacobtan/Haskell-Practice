{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Criterion.Main
newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL go
  where
    go = \case
      (y:ys) -> y : go ys
      [] -> [x]
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = ($[]) . unDL
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
-- | version 2 (slow)
-- snoc xs x = DL $ unDL (singleton x) . unDL xs
-- | version 1 (works)
snoc (ys') y = DL (\_ -> go ys)
  where
    ys = toList ys'
    go = \case
      x:xs -> x : go xs
      [] -> [y]
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
-- | version 2 (slow)
-- append xs ys = DL $ unDL ys . unDL xs
-- | version 1 (works)
append xs' ys' = DL (\_ -> go xs)
  where
    xs = toList xs'
    ys = toList ys'
    go = \case
      [] -> ys
      x:xs -> x: go xs
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

