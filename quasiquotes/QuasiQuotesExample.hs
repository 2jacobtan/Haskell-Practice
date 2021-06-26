-- https://www.youtube.com/watch?v=NrCBCkwOtT0&t=57m34s

{-# LANGUAGE QuasiQuotes #-}

module QuasiQuotesExample where

import MatrixQQ

numbers :: [[Int]]
numbers =
  [matrix|
    1 2 3
    4 5 6
    7 8 9
  |]

-- >>> numbers
-- [[1,2,3],[4,5,6],[7,8,9]]
