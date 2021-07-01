-- http://dev.stephendiehl.com/hask/#metaprogramming

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module CalcQQ where

-- import Language.Haskell.TH.Syntax
--     ( Lift(lift, liftTyped), Exp, Q, location, Loc(loc_filename), liftData )
import Language.Haskell.TH.Syntax
    ( Exp, Q, location, Loc(loc_filename), liftData )
import Language.Haskell.TH.Quote ( QuasiQuoter(QuasiQuoter) )

import Text.Parsec ( eof, (<|>), parse, ParseError )
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Control.Monad.Identity ( Identity )
import Data.Data

data Expr
  = Tr
  | Fl
  | Zero
  | Succ Expr
  | Pred Expr
  deriving (Eq, Show, Data)

-- instance Lift Expr where
--   lift Tr         = [| Tr |]
--   lift Fl         = [| Tr |]
--   lift Zero       = [| Zero |]
--   lift (Succ a)   = [| Succ a |]
--   lift (Pred a)   = [| Pred a |]
--   liftTyped = error "not implemented"

type Op = Ex.Operator String () Identity

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Op a
prefixOp x f = Ex.Prefix (reservedOp x >> return f)

table :: [[Op Expr]]
table = [
    [ prefixOp "succ" Succ
    , prefixOp "pred" Pred
    ]
  ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

true, false, zero :: Parser Expr
true  = reserved "true" >> return Tr
false = reserved "false" >> return Fl
zero  = reservedOp "0" >> return Zero

factor :: Parser Expr
factor =
      true
  <|> false
  <|> zero
  <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = semiSep expr

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"

calcExpr :: String -> Q Exp
calcExpr str = do
  filename <- loc_filename `fmap` location
  case parse (contents expr) filename str of
    Left err -> error (show err)
    -- Right tag -> [| tag |]
    Right tag -> liftData tag

calc :: QuasiQuoter
calc = QuasiQuoter calcExpr err err err
  where err = error "Only defined for values"