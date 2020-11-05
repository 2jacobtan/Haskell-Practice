import Lib
import Data.Monoid (getAll, All(All))

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
  putStrLn "1. Fold the Boolean conjunction operator over the list of results of sequA (applied to some value)."
  print $ foldr (&&) True $ sequA 5
  print $ getAll $ foldMap All $ sequA 5
  p
  putStrLn "2. Apply sequA to s'—you’ll need fromMaybe."
  print $ sequA $ fromMaybe undefined s'
  p
  putStrLn "3. Apply bolt to ys—you’ll need fromMaybe."
  print $ bolt  $ fromMaybe undefined ys
  p
  