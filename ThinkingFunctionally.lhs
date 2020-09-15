
Thinking Functionally with Haskell, p249: State Monad
\begin{code}
import Control.Monad.State
-- import Control.Monad.Fail

data BinTree a = Leaf a | Fork (BinTree a) (BinTree a)

build xs = evalState (build2 (length xs)) xs
build2 :: Int -> State [a] (BinTree a)
build2 1 =
    do
        xs' <- get
        case xs' of
            x:xs -> do
                put xs
                return (Leaf x)
            _ -> error "ran out of items"
build2 n =
    do
        u <- build2 m
        v <- build2 (n-m)
        return (Fork u v)
    where m = n `div` 2

instance Show a => Show (BinTree a) where
    show =
        let
            show' h tree = case tree of
                Leaf a -> " (Leaf " ++ show a ++ ")"
                Fork left right -> "\n" ++ indent h ++ "Fork " ++ show' (h+1) left ++ show' (h+1) right
                where
                    indent_string = "  "
                    indent h = do {[1..h]; indent_string}
        in show' 0

\end{code}
