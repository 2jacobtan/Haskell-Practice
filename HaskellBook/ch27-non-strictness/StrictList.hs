{-# LANGUAGE Strict #-}

module StrictList where

data List a
  = Nil
  | Cons a (List a)
  deriving (Show)

take' n _ | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) =
  (Cons x (take' (n -1) xs))

map' _ Nil = Nil
map' f (Cons x xs) =
  (Cons (f x) (map' f xs))

repeat' x = xs where xs = (Cons x xs)

main = do
  print $ take' 10 $ map' (+ 1) (repeat' 1)

{-  

What will sprint output?

1. _ polymorphic Num a => a

2. ['1'] -- oops it's "1"

3. [_] -- it's _ -- maybe 1:[] is not evaluated because first argument to : is polymorphic Num a => a -- and _:[] makes no sense because the evaluation stopped at first argument.

But:
  Prelude Text.Pretty.Simple Debug.Trace> x = [1,2,id 3, 4, 5] :: [Int]
  Prelude Text.Pretty.Simple Debug.Trace> :sprint x
  x = [1,2,_,4,5]

Okay, probably because the : itself is polymorphic so does not even get tried.
  Prelude Text.Pretty.Simple Debug.Trace> x = [1,2,id 3, 4, 5]
  Prelude Text.Pretty.Simple Debug.Trace> :sprint x
  x = _

Prelude Text.Pretty.Simple Debug.Trace> x = ((:) :: Int -> [Int] -> [Int]) 1 []
Prelude Text.Pretty.Simple Debug.Trace> :sprint x
x = [1]

Prelude Text.Pretty.Simple Debug.Trace> x = ((:) :: Int -> [Int] -> [Int]) (id 1) []
Prelude Text.Pretty.Simple Debug.Trace> :sprint x
x = [_]


4. 1

5. _

6. _ -- because it's still a function application; only fully applied Data constructors (including nullary ones like 1 :: Int) are opportunistically evaluated (opportunistic strictness).

-}

{-

Will printing this expression result in bottom?

1. 1

2. bottom

3. bottom

4. fine because Strict doesn't affect to length defined outside this module

5. fine because Strict doesn't affect const defined outside this module

6. fine as before

7. obviously bottom

-}
p x = print x
p_ = putStrLn ""
ps = putStrLn
strictTest1 = do
  ps "q1"
  p $ snd (undefined, 1)
  -- ps "q2"
  -- let x = undefined
  -- p $ let y = x `seq` 1 in snd (x, y) --q2
  -- ps "q3"
  -- p $ length $ [1..5] ++ undefined
  ps "q4"
  p $ length $ [1..5] ++ [undefined]
  ps "q5"
  p $ const 1 undefined
  -- ps "q5'"
  -- p $ let const' x _ = x in const' 1 undefined
  ps "q6"
  p $ const 1 (undefined `seq` 1)
