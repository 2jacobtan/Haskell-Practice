-- continued in CCLAW sandbox

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

data Mark = O | X | E -- E == empty box
  deriving Eq
type Board = [Mark]

isGameOver :: Board -> Bool
isGameOver board = isWon O || isWon X
  where
    isWon mark = any (isWonLine mark) $ concat [rows, cols, diag1, diag2]
    isWonLine mark line = all ((==mark).(board !!)) line
    rows = [[i,i+1,i+2] | i <- [0,3,6]]
    cols = [[i,i+3,i+6] | i <- [0,1,2]]
    diag1 = [[0,4,8]]
    diag2 = [[2,4,6]]

countValidStates :: [Board] -> Int
countValidStates boards = undefined