For testing purposes.

\begin{code}
{-# LANGUAGE ExistentialQuantification #-}

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
tco' []  y = y
tco' (x:xs) y =
    let z' =  compare2 x y
    in z' `seq` tco xs z'

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
