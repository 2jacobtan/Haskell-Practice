import WriteState

main = do
  print $ runState get "curryIsAmaze"


get :: State s s
get = state $ \s -> (s,s)

put :: s -> State s ()
put s = state $ \_ -> ((),s)

exec :: State s a -> s -> s
exec m = snd . runState m

eval = evalState

modify :: (s -> s) -> State s ()
modify f = get >>= put . f