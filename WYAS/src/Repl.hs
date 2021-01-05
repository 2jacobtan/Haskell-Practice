module Repl where

import System.IO (stderr, hPutStrLn,  hFlush, stdout )
import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Data.Function ((&))
import Control.Monad.Except (MonadTrans(lift))

import Types
import Parsing ( extractValue, readExpr, trapError )
import Evaluation ( eval )
import VarsAndAssignment (bindVars,  nullEnv, liftThrows, runIOThrows )
import DefiningSchemeFunctions

-- Bulding a REPL

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = expr & (liftThrows . readExpr >=> eval env) <&> show & runIOThrows

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env = evalString env >=> putStrLn

until_ :: Monad m => (t -> Bool) -> m t -> (t -> m a) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint =<< primitiveBindings 

runOne :: [String] -> IO ()
runOne args = do
  env <- ($ [("args", List $ String <$> drop 1 args)])
    . bindVars =<< primitiveBindings
  hPutStrLn stderr =<< runIOThrows (show <$> eval env (List [Atom "load", String (head args)]))


