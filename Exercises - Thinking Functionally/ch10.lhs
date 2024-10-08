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


Exercise B
\begin{code}

add3 (Just x) (Just y) (Just z) = Just (x + y + z)
add3 _ _ _ = Nothing

add3' x' y' z' = do x <- x'
                    y <- y'
                    z <- z'
                    pure $ x + y + z
\end{code}


Exercise D
\begin{code}

fmap f = (>>= pure . f)
liftM f = (>>= pure . f)

join :: Monad m => m (m a) -> m a
join = (>>= id)

bind x f = Main.join $ Main.liftM f x
\end{code}


Exercise E
\begin{code}

sequence_' :: Monad m => [m a] -> m ()
sequence_' = foldr (>>) (pure ())

sequence' :: Monad m => [m a] -> m [a]
sequence' = foldr (\mx mxs -> mx >>= \x -> mxs >>= \xs -> pure (x:xs)) (pure [])

mapM_' f = sequence_' . map f

mapM f = sequence' . map f

foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
-- foldM f b = foldl (\mb a -> mb >>= \b -> f b a) (pure b)
foldM f e [] = pure e
foldM f e (x:xs) = f e x >>= \y -> Main.foldM f y xs 

for_ :: Monad m => [a] -> (a -> m b) -> m ()
-- for_ xs f = sequence_' $ map f xs
for_ = flip mapM_'

\end{code}


Exercise H

liftM f mx
= mx >>= \x -> return (f x)
= id mx >>= return . f
= id >=> return . f

join
= mmx >>= \mx -> mx
= id mmx >>= id
= id >=> id

(join . liftM join) mmx
= join $ liftM join mmx
= join $ mx
= x

4th rule:
LHS mmx
= liftM f (join mmx)
= liftM f (mx)
= m(fx)

RHS mmx
= join (liftM (liftM f) mmx)
= join $ m(liftM f mx)
= join $ m(m(fx))
= m(fx)


Exercise I
infinite loop


Exercise J
\begin{code}


hangman_ :: IO ()
hangman_ = do {
  putStrLn "To quit, type ':q'";
  wordsString <- readFile "Words";
  -- putStrLn $ show $ words wordsString;
  foldr hangman (pure ()) (words wordsString);
  putStrLn "Goodbye.";
  pure ()
}
  where
    hangman :: String -> IO () -> IO ()
    hangman word doRest = do {
      putStrLn "Guess my word:";
      putStrLn $ do {word; "_"};
      toContinue <- hangman1 [];
      if toContinue then doRest else pure ()
    }
      where
        hangman1 :: [Char] -> IO (Bool)
        hangman1 current = do {
          putStr "Guess: ";
          guessInput <- getLine;
          (case guessInput of
            ":q" -> pure False
            guess -> do {
              putStrLn $ map (\c -> if c `elem` current'guess then c else '_') word;
              (case unguessed of
                [] -> putStrLn "You got it!" >> pure True
                _ -> hangman1 $ intersection
              );
            } where
                current'guess = guess ++ current
                intersection = (current'guess) `intersect` word
                unguessed = word \\ intersection
          );
        }
\end{code}


Exercise K
\begin{code}

fib n = runST $ fibST n

fibST n = do {
  a <- newSTRef (0,1);
  foldr (>>) (pure()) (replicate n $ do {
    (x,y) <- readSTRef a;
    let x'y = x + y
    in x'y `seq` writeSTRef a (y, x+y);
  });
  (ans,_) <- readSTRef a;
  pure ans
}
\end{code}


Exercise L
\begin{code}

gcd1 (x,y) = evalState gcdState (x,y)
-- gcdState :: State (Int,Int) Int
gcdState = do {
  (x,y) <- get;
  (if
      | x == y -> pure x
      | x < y -> do {
        let y'x = y - x in y'x `seq` put (x, y'x);
        gcdState
      }
      | x > y -> do {
        let x'y = x - y in x'y `seq` put (x'y, y);
        gcdState
      }
      | otherwise -> pure 0
  )
}

gcd2 (x,y) = runST $ gcdST (x,y)
-- gcdState :: State (Int,Int) Int
gcdST (x,y) = do {
  a <- newSTRef x;
  b <- newSTRef y;
  let loop = do {
    x <- readSTRef a;
    y <- readSTRef b;
    (if
        | x == y -> pure x
        | x < y -> do {
          let y'x = y - x in y'x `seq` do {
            writeSTRef b y'x
          }; loop
        }
        | x > y -> do {
          let x'y = x - y in x'y `seq` do {
            writeSTRef a x'y
          }; loop
        }
        | otherwise -> pure 0
    )
  } in loop
}

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


solve :: State_ -> ST s (Maybe (State_, [Int]))
solve initialState = do {
  encounteredRef <- newSTRef (Set.singleton initialState);
  frontierRef <- newSTRef [(initialState,
    [let Just x = elemIndex 0 . elems $ initialState in x :: Int])];
  nextFrontierRef <- newSTRef [];
  
  let solve1 = do {
    front <- readSTRef frontierRef;
    nextFront <- readSTRef nextFrontierRef;
    encountered <- readSTRef encounteredRef;

    case (front, nextFront) of
      ([],[]) -> return Nothing -- no possible solution
      _ -> do {
        --  if frontier is empty, swap with nextFrontier
        if -- Debug.Trace.trace (show $ front == []) $
        front == []
        then do
          writeSTRef frontierRef (reverse nextFront)
          writeSTRef nextFrontierRef []
        else pure ();
        
        front <- readSTRef frontierRef;
        nextFront <- readSTRef nextFrontierRef;
        case front of
          (s,m:ms) : xs
            | solved s -> Debug.Trace.trace
                ("Positions covered: " ++ (show . length) encountered) $
                return $ Just (s,m:ms)
            | otherwise ->
              let nextMoves =
                    [(nextState, m1:m:ms)
                    | m1 <- moves m, let nextState = move s m m1
                    , not $ nextState `Set.member` encountered
                    ]
              in do {
                writeSTRef encounteredRef $
                  encountered `Set.union` (Set.fromList $ map fst nextMoves);
                writeSTRef frontierRef $ xs;
                writeSTRef nextFrontierRef $ nextMoves ++ nextFront;
-- debug
-- Debug.Trace.trace ("front " ++ show (length front)) $ pure ();
-- Debug.Trace.trace ("nextFront " ++ show (length nextFront)) $ pure ();
-- -- Debug.Trace.trace ("nextFront\n" ++ show (Grid $ elems $ fst $ head $ nextFront)) $ pure ();
-- Debug.Trace.trace ("encountered " ++ show (length encountered)) $ pure ();
                
                solve1
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
    Just x -> print x
  putStrLn $ "startState was " ++ (show . elems) startState
--  solver

main = solver

\end{code}
