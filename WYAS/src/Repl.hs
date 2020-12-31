module Repl where

import System.IO ( hFlush, stdout )
import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Data.Function ((&))
import Control.Monad.Except (MonadTrans(lift))

import Types ( Env )
import Parsing ( extractValue, readExpr, trapError )
import Evaluation ( eval )
import VarsAndAssignment ( nullEnv, liftThrows, runIOThrows )
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


-- Adding Variables and Assignment
runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr
