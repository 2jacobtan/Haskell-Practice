-- From Okasaki's Purely
-- Functional Data Structures

import Criterion.Main
data Queue a = Queue
  { enqueue :: [a],
    dequeue :: [a]
  }
  deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push x xs@(Queue en _) = xs {enqueue = x : en}

pop :: Queue a -> Maybe (a, Queue a)
pop xs = case de of
  y : ys -> Just (y, Queue en ys)
  [] -> case en of
    zs@(_ : _) ->
      let (z' : zs') = foldl (flip (:)) [] zs
       in Just (z', Queue [] zs')
    [] -> Nothing
  where
    en = enqueue xs
    de = dequeue xs

pushList x xs = xs ++ [x]
popList [] = Nothing
popList (x:xs) = Just (x, xs)

listBench n0 l = case n0 of
  0 -> l
  n ->
    let Just (_,xs) = popList . pushList 1 $ l
     in listBench (n-1) xs

queueBench n0 q = case n0 of
  0 -> q
  n ->
    let Just (_,xs) = pop . push 1 $ q
     in queueBench (n-1) xs

main = defaultMain
  [ bench "list" $
      whnf (listBench 1234) [1..999],
    bench "queue" $
      whnf (queueBench 1234) (Queue [1..999] [])
  ]