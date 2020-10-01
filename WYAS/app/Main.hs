{-# LANGUAGE LambdaCase #-}
module Main where

import Lib
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad (liftM)
import Numeric (readDec, readHex, readOct, readInt)
import Data.Char (digitToInt)
import Text.Parsec (parserZero)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr

spaces :: Parser ()
spaces = skipMany1 space

data LispVal 
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Character Char
  | Bool Bool deriving Show

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapedChars <|> noneOf "\"\\")
  char '"'
  return $ String x
  where
    escapedChars = escapedChars1
    escapedChars' :: Parser Char -- answer
    escapedChars' = char '\\' >> oneOf "\\\"nrt"
      >>= \x -> return $ case x of
        '\"' -> '\"'
        '\\' -> '\\'
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
    escapedChars1 :: Parser Char
    escapedChars1
      = char '\\'
      >> (
        (char '\\') -- >> pure '\\')
        <|> (char '"') -- >> pure '"')
        <|> (char 'n' >> pure '\n')

      )
      -- This version doesn't work because megaparsec's <|> will only try the alternative if the original fails witohut consuming any input!
    escapedChars2 :: Parser Char
    escapedChars2
      = (string "\\\\" >> pure '\\')
      <|> (string "\\\"" >> pure '\"')
      <|> (string "\\n" >> pure '\n')
      -- This version works using "try" to "pretends that it hasn't consumed any input when an error occurs."
    escapedChars3 :: Parser Char
    escapedChars3
      = try (string "\\\\" >> pure '\\')
      <|> try (string "\\\"" >> pure '\"')
      <|> (string "\\n" >> pure '\n')

parseAtom :: Parser LispVal
parseAtom = do 
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
  char '#'
  b <- oneOf "tf"
  return . Bool $ case b of
    't' -> True
    'f' -> False
    
parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do {
  char '#';
  r <- oneOf "bodx";
  let (digit',read') = case r of {
    'b' -> (oneOf "01", readInt 2 (const True) digitToInt);
    'o' -> (octDigit, readOct);
    'd' -> (digit, readDec);
    'x' -> (hexDigit, readHex);
    } in do
  sign <- option (1) (char '-' >> return (-1))
  num <- many1 digit';
  return . Number . (sign *) . fst . head . read' $ num;

  } <|> (liftM (Number . read) $ many1 digit)

parseChar :: Parser LispVal
parseChar = do
  -- try $ string "#\\"
  char '#' >> char '\\'
  c <- choice
    [ try $ string "space" >> return ' '
    , try $ string "newline" >> return '\n'
    , do {x <- anyChar; notFollowedBy alphaNum; return x}
    ]
  return $ Character c

parseExpr :: Parser LispVal
parseExpr = choice
  [ parserZero
  , parseAtom
  , parseString
  , try parseNumber
  , try parseBool
  , try parseChar
  ]