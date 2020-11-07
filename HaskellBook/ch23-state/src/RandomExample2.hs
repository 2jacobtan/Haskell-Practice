{-# LANGUAGE BangPatterns #-}
module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import RandomExample
import qualified Debug.Trace as Debug
-- Six-sided die

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie2 :: State StdGen Die
rollDie2 = state $
  (\(n,s) -> (intToDie n, s)) . randomR (1,6)

rollDie3 :: State StdGen Die
rollDie3 = state $
  uncurry ((,) . intToDie) . randomR (1,6)


rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes'
  :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

-- Seems appropriate?
-- repeat :: a -> [a]
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- replicateM :: Monad m
--   => Int -> m a -> m [a]
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie


rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die)
          (count + 1) nextGen 

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n gen = evalState (go 0 0) gen
  where
    newRoll = state $ randomR (1,6)
    go :: Int -> Int -> State StdGen Int
    go !sum !count
      | sum >= n = return count
      | otherwise = do
          roll <- newRoll
          roll <- pure $ Debug.trace (show roll) roll -- debug only
          go (sum + roll) (count + 1)
