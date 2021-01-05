{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.IORef (IORef)
import Data.Array (Array)
import Data.Complex (Complex)
import Text.Parsec (ParseError)
import Control.Monad.Except (ExceptT)
import System.IO (Handle)

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ExceptT LispError IO

data LispVal 
  = Atom String
  | Vector (Array Int LispVal)
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Double
  | Ration Rational
  | Compl (Complex Double)
  | String String
  | Character Char
  | Bool Bool
-- Defining Scheme Functions
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {
      params :: [String], vararg :: Maybe String,
      body :: [LispVal], closure :: Env
  }
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle


-- Evaluation, Part 1

showVal :: LispVal -> String
showVal = \case
  (String contents) -> "\"" ++ contents ++ "\""
  (Atom name) -> name
  (Number contents) -> show contents
  (Bool True) -> "#t"
  (Bool False) -> "#f"
  (List contents) -> "(" ++ unwordsList contents ++ ")"
  (DottedList head tail) -> "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
  (PrimitiveFunc _) -> "<primitive>"
  Func {..} ->
    "(lambda (" ++ unwords (map show params) ++
      (case vararg of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal


data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected
    ++ " args; found values "
    ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected
    ++ ", found "
    ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError
