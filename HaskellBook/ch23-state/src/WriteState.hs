module WriteState (State, state, evalState, runState)
where

-- main =
--   functorTest
--   >> applicativeTest
--   where
--     functorTest = do
--       let f = (+1) <$> (State $ \s -> (0, s))
--       print $ runState f 0
--     applicativeTest = do
--       let
--         applied =
--           State (\s -> ((3*),s+1))
--           <*> State (\s -> (2^s,s+1))
--       print $ runState applied 0
--       print $ runState applied 1

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
  fmap f (State g) = State $ do
    (a,s) <- g
    return (f a, s)



instance Applicative (State s) where
  pure a = State $ \s -> (a,s)
  (<*>) (State f) (State g) = State $
    \s ->
      let (h, s') = f s in
      let (x, s'') = g s' in
      (h x, s'')


instance Monad (State s) where
  return = pure
  (State f) >>= g = State $
    \s ->
      let (x,s') = f s
      in runState (g x) s'


state = State

evalState m s0 = fst $ runState m s0