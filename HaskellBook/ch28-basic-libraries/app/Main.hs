{-# LANGUAGE TypeApplications #-}

import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

unboxed _ = VU.fromList @Int [(0) .. 9999]
{-# SCC unboxed #-}

boxed _ = V.fromList @Int [0 .. 9999]
{-# SCC boxed #-}

main = do
  defaultMain
    [ 
      bench "bench2" $ whnf print $
        {-# SCC printBoxed #-} (boxed () ) V.! 2,
      bench "bench1" $ whnf print $
        {-# SCC printUnboxed #-} (unboxed () ) VU.! 1
    ]
