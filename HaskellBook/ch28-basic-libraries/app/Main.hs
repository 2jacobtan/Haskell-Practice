{-# LANGUAGE TypeApplications #-}

import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU


main = do
  defaultMain
    [ bench "bench1" $ whnf print $ 1
    ]
  
  let unboxed _ = VU.fromList @Int [(0) .. 9999]
      {-# SCC unboxed #-}
  
  print $ {-# SCC "printUnboxed" #-} (unboxed ()) VU.! 1

  defaultMain
    [ bench "bench2" $ whnf print $ 2
    ]
  
  print $ {-# SCC "printUnboxed" #-} (unboxed ()) VU.! 1
  
  
  defaultMain
    [ bench "bench3" $ whnf print $ 3
    ]
  
  
  let boxed _ = V.fromList [0 .. 9999]
      {-# SCC boxed #-}
  
  print $ {-# SCC printBoxed #-} (boxed ()) V.! 2
  
  defaultMain
    [ bench "bench4" $ whnf print $ 4
    ]
  
  print $ {-# SCC printBoxed #-} (boxed ()) V.! 2
