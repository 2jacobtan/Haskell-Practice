import Lib

p = putStrLn ""

main :: IO ()
main = do
  print x1
  print x2
  print $ x3 3
  p
  print $ bolt 5
  p
  print $ fromMaybe 0 xs
  print $ fromMaybe 0 zs
  p
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  p
  print $ sequence [(>3), (<8), even] 7
  p
  -- print $