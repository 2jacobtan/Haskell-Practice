module Loci where

import Control.Monad

blah :: [Integer]
blah = [0 .. 999]

main :: IO ()
main =
  replicateM_ 10000 (putStr . show $ blah !! 0)