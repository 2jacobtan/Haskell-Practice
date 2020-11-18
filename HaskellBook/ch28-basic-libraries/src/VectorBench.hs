{-# LANGUAGE TypeApplications #-}

module VectorBench where

import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- | just testing type applications
-- fun :: a -> a -> a
fun :: a -> b -> a
fun x _ = x

funxy = fun @Int @Int 1 2

boxed = {-#SCC boxed #-} V.fromList [0 .. 9999]

unboxed = {-#SCC unboxed #-} VU.fromList @Int [(0) .. 9999]

main =
  defaultMain
    [ bench "boxed" $ whnf print $ boxed V.! 1,
      bench "unboxed" $ whnf print $ unboxed VU.! 1
    ]