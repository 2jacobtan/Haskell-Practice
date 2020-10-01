{-# LANGUAGE LambdaCase #-}
module Main where

import Lib
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad (liftM)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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
  return $ case atom of 
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber