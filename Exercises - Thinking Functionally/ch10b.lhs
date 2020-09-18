\begin{code}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
import Control.Monad.State
import Data.List (elemIndex, (\\), intersect)
import Control.Monad.ST
import Data.STRef
import Data.Array
import qualified Data.Set as Set
import qualified Debug.Trace
import Data.Function ((&))

\end{code}


Exercise M
\begin{code}
newtype Grid = Grid {unGrid :: [Int]}
instance Show Grid where
  show (Grid xs) = go xs
    where
      go [] = ""
      go xs = show (take 3 xs) ++ "\n" ++ go (drop 3 xs)

type State_ = Array Int Int

initialState' = listArray (0,8) $ [0..8] :: State_ -- 0 represents the blank space
winState = listArray (0,8) $ [1..8] ++ [0] :: State_
winState' = listArray (0,8) $ [1,2,3,4,5,6,8,7,0]
solved = \x -> x == winState || x == winState'

rows = listArray (0,2) [
  [i..(i+2)] | i <- [0,3,6]
  ]
cols = listArray (0,2) [
  [i,(i+3),(i+6)] | i <- [0..2]
  ]

getRowAdjacents i = intersect [i-1,i+1] $ rows ! (i `div` 3)
getColAdjacents i = intersect [i-3,i+3] $ cols ! (i `mod` 3)
getAdjacents i = getRowAdjacents i ++ getColAdjacents i
showAdjacentsList xs = putStr $ foldr (\x acc -> x ++ "\n" ++ acc) ""
    [ concat [if i `elem` xs then show i else "-" | i <- r] | r <- elems rows]
showAdjacents = showAdjacentsList . getAdjacents

moves :: Int -> [Int]
moves = getAdjacents
move fromState fromI toI = fromState // [(toI,0),(fromI,fromState ! toI)] :: State_

limit = 5
solve :: State_ -> ST s (Maybe (State_, [Int]))
solve initialState = do {
  encounteredRef <- newSTRef (Set.singleton $ elems initialState);
  frontierRef <- newSTRef [(initialState,[0])]; -- found the bug! first-move wrongly hard-coded to 0
  nextFrontierRef <- newSTRef [];

  count <- newSTRef 0;
  
  let solve1 = do {
    front <- readSTRef frontierRef;
    nextFront <- readSTRef nextFrontierRef;
    encountered <- readSTRef encounteredRef;

    case (front, nextFront) of
      ([],[]) -> return Nothing -- no possible solution
      _ -> do {
        --  if frontier is empty, swap with nextFrontier
        if Debug.Trace.trace (show $ front == []) $ front == []
        then do
          writeSTRef frontierRef (reverse nextFront)
          writeSTRef nextFrontierRef []
        else pure ();
        
        front <- readSTRef frontierRef;
        nextFront <- readSTRef nextFrontierRef;
        case front of
          (s,m:ms) : xs
            | solved s -> return $ Just (s,m:ms)
            | otherwise ->
              let nextMoves =
                    [(nextState, m1:m:ms)
                    | m1 <- moves m, let nextState = move s m m1
                    , not $ (elems nextState) `Set.member` encountered
                    ]
              in do {
                writeSTRef encounteredRef $
                  encountered `Set.union` (Set.fromList $ map (elems . fst) nextMoves);
                writeSTRef frontierRef $ xs ++ nextMoves;
                -- writeSTRef nextFrontierRef $ nextMoves ++ nextFront;
                count' <- readSTRef count;
                writeSTRef count (count'+1);
-- debug
Debug.Trace.trace ("front " ++ show (front)) $ pure ();
Debug.Trace.trace ("nextFront " ++ show (length nextFront)) $ pure ();
-- Debug.Trace.trace ("nextFront\n" ++ show (Grid $ elems $ fst $ head $ nextFront)) $ pure ();
Debug.Trace.trace ("encountered " ++ show (encountered)) $ pure ();
                
                if count' < limit then solve1 else return Nothing
              }
          _ -> error "rip"
      }

  } in solve1;
}

solver = do
  putStrLn "Starting positions separated by space:"
  userInput <- getLine
  let startState = userInput & words & map read & listArray (0,8)
  case runST $ solve $ startState of
    Nothing -> do
      putStrLn "No solution found."
      putStrLn $ "startState was " ++ (show . elems) startState
    Just x -> print x
  solver

main = solver

\end{code}
