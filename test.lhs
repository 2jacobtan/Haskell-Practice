For testing purposes.

\begin{code}
{-# LANGUAGE ExistentialQuantification #-}

import Data.Function ((&))
main = print "Hello world!"

\end{code}


Existential types
\begin{code}

data T = forall a. Show a => T a

tList =
    [ T 1
    , T 'a'
    , T True
    ]

printT = mapM_ (\(T x) -> print x) tList

-- >>> :type printT
-- printT :: IO ()

\end{code}


"Let" syntax
\begin{code}
letTest = 2 +
    let
    x = 1
    in
    x

\end{code}


DaPhone saga
\begin{code}

largest :: Ord a => [a] -> a
largest [x] = x
largest (x:xs) =
    if (largest xs) > x
    then largest xs
    else x

largest' :: Ord a => [a] -> a
largest' = foldr1 (\x acc -> if acc > x then acc else x)

largest'2 :: Ord a => [a] -> a
largest'2 [x] = x
largest'2 (x:xs) =
    let largestOthers = largest'2 xs in
    if largestOthers > x
    then largestOthers
    else x

largest'3 :: Ord a => [a] -> a
largest'3 [x] = x
largest'3 (x:xs) =
    if largestOthers > x
    then largestOthers
    else x
    where
        largestOthers = largest'3 xs

largest'4 :: Ord a => [a] -> a
largest'4 [x] = x
largest'4 (x:xs) = foldr1_clone largerOf2 (x:xs)
    where
        foldr1_clone _ (x:[]) = x
        foldr1_clone f (x:xs) = x `f` foldr1_clone f xs
        largerOf2 x y = if x > y then x else y

largest'4' :: Ord a => [a] -> a
largest'4' [x] = x
largest'4' (x:xs) = foldr1_clone largerOf2 (x:xs)
    where
        foldr1_clone _ (x:[]) = x
        foldr1_clone f (x:xs) = x `f` foldr1_clone f xs
        largerOf2 x y = if x > y then x else y
        
usingMax [x] = x
usingMax (x:xs) = max (usingMax xs) x

usingMax' (x:xs) = max (usingMax xs) x

tco []    y = y
tco (x:xs) y = tco xs (if x > y then x else y)

compare2 :: (Ord a) => a -> a -> a
compare2 = (\x y -> if x > y then x else y)
-- tco' :: Ord a => a -> [a] -> a
tco' y [] = y
tco' y (x:xs) =
    let z' =  compare2 x y
    in z' `seq` tco' z' xs

\end{code}


Thinking Functionally with Haskell, p76, merg sort
\begin{code}

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

merge' :: (Ord a) => [a] -> [a] -> [a]
merge' [] ys = ys
merge' xs [] = xs
merge' xs'@(x:xs) ys'@(y:ys)
    | x <= y = x:merge' xs ys'
    | otherwise = y:merge' xs' ys

\end{code}


\begin{code}

data BinTree a = Leaf | Node a (BinTree a) (BinTree a) deriving Show
myTree =
    Node 3
        (Node 1
            (Node 0 Leaf Leaf)
            (Node 2 Leaf Leaf))
        (Node 4 Leaf Leaf)

f :: Show a => a -> String
f = show

traverseMap Leaf = Leaf
traverseMap (Node x left right) =
    Node (f x) (traverseMap left) (traverseMap right)

-- instance Show a => Show (BinTree a) where
--     show =
--         let
--             show' h tree = case tree of
--                 Leaf -> " Leaf"
--                 Node x left right -> "\n" ++ indent h ++ "Node " ++ show x ++ show' (h+1) left ++ show' (h+1) right
--                 where
--                     indent_string = "  "
--                     indent h = do {[1..h]; indent_string}
--         in show' 0

\end{code}


\begin{code}

data ValList a = V a | L [ValList a] deriving Show

flat_list :: ValList Integer
flat_list = L [V 3, V 2, V 1 ]

nested_list :: ValList a
nested_list = L [L []]

arbitrarily_nested_list :: ValList Integer
arbitrarily_nested_list = L [V 1, L [V 2, V 3], L [L [V 4, V 5]]]

-- https://smucclaw.slack.com/archives/C01B92UL9B4/p1601049294002500?thread_ts=1600884447.000800&cid=C01B92UL9B4
data Cell a = Nil | C a deriving Eq

example1 = (Nil, (Nil, Nil)) -- :: (Cell String, (Cell Integer, Cell ()))

example2 = (C "1", (C 2, Nil))

type CellsType = (Cell String, (Cell Integer, Cell()))
e1_equals_e2 = example1 == (example2 :: CellsType)

equals x y = x == (y :: CellsType)
e1_equals'_e2 = example1 `equals` example2

-- e1_equals''_e2 = example1 == example2
-- Error: Ambiguous type variable ‘a0’ arising from a use of ‘==’
--  prevents the constraint ‘(Eq a0)’ from being solved.

example02 = (C "1", (C 2, Nil :: Cell ()))

e1_equals_e02 = example1 == example02

\end{code}


https://smucclaw.slack.com/archives/C019BR4LX9V/p1601351544001400
\begin{code}

multInt x y = go y 0
  where go y' accum = case y' of
          0 -> accum
          _ -> go (y' - 1) (x + accum)

\end{code}


\begin{code}

-- | first attempt at Meng's challenge
-- wrong order
_ = foldr ((.) . dropWhile . (==)) id " /_" "    ////___aoeu"
-- | correct order with `flip`
_ = foldr ((flip (.)) . dropWhile . (==)) id " /_" "    ////___aoeu"

-- | point-full
_ = foldr (\x xs -> xs . dropWhile (==x)) id " /_" "  ////__aoeu"
_ = foldl (\xs x -> dropWhile (==x) . xs) id " /_" "  ////__aoeu"

-- | one-point foldl
_ = foldl (\xs -> (. xs) . dropWhile . (==)) id " /_" "    ////___aoeu"
-- | point-free foldl
_ = foldl (( . dropWhile . (==)) . flip (.)) id " /_" "    ////___aoeu"

-- | eta reduction for foldl
-- \xs -> \x -> dropWhile (==x) . xs
-- \xs -> (. xs) . dropWhile . (==)
-- \xs -> (flip (.) xs) . dropWhile . (==)
-- ( . dropWhile . (==)) . flip (.)

-- | clear version
_ = 
  -- create a list of dropWhile functions for each character
  map (dropWhile . (==)) " /_"
  -- compose them in the correct order
  & foldr (flip (.)) id
  -- apply to input string
  $ "    ////___aoeu"

\end{code}
