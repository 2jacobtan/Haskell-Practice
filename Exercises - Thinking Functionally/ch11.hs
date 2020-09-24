{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data.Fixed (mod')
-- import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (MonadPlus, liftM, ap)
import Data.Char (isSpace, isDigit)
import GHC.Show (showSpace)

-- Exercise A

newtype Angle = Angle Double deriving Show

instance Eq Angle where
  Angle a == Angle b
    = a `mod'` (2*pi) == b `mod'` (2*pi)

newtype Angle2 = Angle2 Double deriving Show

instance Eq Angle2 where
  Angle2 a == Angle2 b
    = reduce a == reduce b
    where
    reduce x
      | x > r = reduce (x - r)
      | x < 0 = reduce (x + r)
      | otherwise = x
      where r = 2*pi


-- Exercise B

newtype Parser a = Parser (String -> Maybe (a,String))

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return x = Parser $ \s -> Just (x, s)
  Parser f >>= g
    = Parser $ \s -> case f s of
    Nothing -> Nothing
    Just (x, s') ->
      let Parser p = g x
      in p s'

-- Exercise C

-- fail = \s -> []
-- fail >> p = \s0 ->
--   case (\s -> []) s0 of
--     [] -> []
--     -- ...

-- hence: fail >> p = \s0 -> [] = fail


-- Exercise D

-- if both p and q would parse successfully, then
-- (p <|> q)
-- = Parser $ \s -> parse p s ++ parse q s
-- = Parser $ \s -> [(a,s)] ++ [(b,t)]
-- = Parser $ \s -> [(a,s),(b,t)]

-- limit (Parser pq) = Parser $ \s -> case pq s of
--   [] -> []
--   x -> head x


-- Exercise E

-- instance MonadPlus Parser where
--   mzero = Parser $ \s -> [] -- = fail
--   p `mplus` q = Parser $ \s -> case apply p s of
--     Nothing -> apply q s
--     Just x -> x


-- Execrise F

-- if p is Just x, and Just x >>= f is Nothing, but q >>= f is Just y


-- Exercise G

failP :: Parser a
failP = Parser $ \s -> Nothing
apply :: Parser a -> String -> Maybe (a, String)
apply (Parser p) s = p s
none :: Parser [a]
none = return []

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser $ \s -> case apply p s of
  Nothing -> apply q s
  Just x -> Just x


manyP :: Parser a -> Parser [a]
manyP p = (do
  xs <- some p
  return $ xs) <|> none

some :: Parser a -> Parser [a]
some p = do
  x <- p
  xs <- manyP p
  return $ x:xs

predP pred p = do
  x <- p
  if pred x then return x else failP

getC = Parser $ \xs -> case xs of
  x:xs' -> Just (x,xs')
  _ -> Nothing

digit = predP isDigit getC

space = manyP (predP isSpace getC) >> return ()

token p = space >> p

parseFloat :: Parser Double
parseFloat = token $ do
  nString <- some digit
  let n = read nString :: Double
  predP (== '.') getC
  mString <- some digit
  let m = read mString :: Double
  return $ n + m / fromIntegral (10^length mString)
  

-- Exercise I
char :: Char -> Parser ()
char c = predP (== c) getC >> return ()

paren p = (do
  token $ char '('
  x <- token $ p
  token $ char ')'
  return x)

parseCInt :: Parser Expr
parseCInt = do
  nString <- some digit
  return $ C $ CInt (read nString :: Int)

data Op = Plus | Minus | Mul | Div deriving Show
pOp = token $ do {char '+'; return Plus} <|> do {char '-'; return Minus}

data Constant = CInt Int deriving Show
pCons = parseCInt

data Expr = C Constant  | ExprBin Op Expr Expr deriving Show

pExpr = token $ pBinary <|> pTerm -- <|> 
  where
    pTerm = token $ pCons <|> paren pExpr
    pBinary =  do
        t1 <- pTerm
        oper <- pOp
        t2 <- pTerm
        return (ExprBin oper t1 t2)


-- Exercise J

term = token $ pCons <|> paren pExpr
op = pOp
many = manyP
expr = do
  e1 <- term
  pes <- many (pair op term)
  return (foldl shunt e1 pes)

pair p q = do
  vp <- p
  vq <- q
  return (vp,vq)

shunt e1 (p,e2) = ExprBin p e1 e2


-- Exercise K

parenOp p = paren p <|> p

expr' = token (term' >>= rest)

rest e1 = do {
  p <- addop;
  e2 <- term';
  rest (ExprBin p e1 e2)} <|> return e1

term' = token (factor >>= more)

more e1 = do {
  p <- mulop;
  e2 <- factor;
  more (ExprBin p e1 e2)} <|> return e1

factor = token (pCons <|> paren expr)

addop = token $ do {
  char '+';
  return Plus
} <|> do {
  char '-';
  return Minus
}
  
mulop = token $ do {
  char '*';
  return Mul
} <|> do {
  char '/';
  return Div
}


-- Exercise L

isMulOp = \case
  Mul -> True
  Div -> True
  _ -> False

-- instance Show Expr where
--   show e = showsF (\_ -> False) e ""
--     where
--     showsF :: (Op -> Bool) -> Expr -> ShowS
--     showsF _ (C (CInt x)) = showString $ show x
--     showsF f (ExprBin p e1 e2) = showParen (f p) $
--       showsF f1 e1 . showSpace
--       . showop p . showSpace
--       . showsF f2 e2
--       where
--       f1 q = if isMulOp p && not (isMulOp q) then True else False
--       -- f1 = if
--       --   | isMulOp p -> \q -> if
--       --     | isMulOp q -> False
--       --     | otherwise -> True
--       --   | otherwise -> \q -> if
--       --     | isMulOp q -> False
--       --     | otherwise -> False
--       f2 q = if not (isMulOp p) && isMulOp q then False else True
--       -- f2 = if
--       --   | isMulOp p -> \q -> if
--       --     | isMulOp q -> True
--       --     | otherwise -> True
--       --   | otherwise -> \q -> if
--       --     | isMulOp q -> False
--       --     | otherwise -> True
--       showop = \case
--         Mul -> showChar '*'
--         Div -> showChar '/'
--         Plus -> showChar '+'
--         Minus -> showChar '-'

