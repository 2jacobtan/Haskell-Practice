\begin{code}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
import Control.Monad.State
import Data.List ((\\), intersect)
import Control.Monad.ST
import Data.STRef
import Data.Array

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
newtype Grid = Grid [Int]
instance Show Grid where
  show (Grid xs) = foldr (\x acc -> x ++ "\n" ++ acc) ""
    [ concat [if i `elem` xs then show i else "-" | i <- r] | r <- elems rows]

initialState = listArray (0,8) $ [0..8] -- 0 represents the blank space
rows = listArray (0,3) [
  [i..(i+2)] | i <- [0,3,6]
  ]
cols = listArray (0,3) [
  [i,(i+3),(i+6)] | i <- [0..2]
  ]

getRowAdjacents i = intersect [i-1,i+1] $ rows ! (i `div` 3)
getColAdjacents i = intersect [i-3,i+3] $ cols ! (i `mod` 3)
getAdjacents i = Grid $ getRowAdjacents i ++ getColAdjacents i
adjacents i = listArray (0,8) [getAdjacents i | i <- [0..8]]

\end{code}
