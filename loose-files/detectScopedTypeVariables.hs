{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

f :: forall a. (Show a, Num a) => a -> String
f _ = show (g 5)
  where
  g :: a -> a
  g y = y

main :: IO ()
main = do
  case length (f (0 :: Double)) of
    1 -> putStrLn "Scoped type variables is disabled"
    3 -> putStrLn "Scoped type variables is enabled"
    _unexpected -> putStrLn "unexpected behaviour"
  print (f (0::Double))
