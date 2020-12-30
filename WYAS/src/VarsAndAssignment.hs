{-# LANGUAGE TupleSections #-}

module VarsAndAssignment where

import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.IORef ( newIORef, readIORef, writeIORef, IORef )
import Control.Monad.Except
    ( MonadError(throwError), ExceptT, runExceptT, MonadTrans(lift) )
import Data.Maybe (isJust)

import Parsing

-- Adding Variables and Assignment

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> isJust . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- lift $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
    (lift . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- lift $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
    (lift . flip writeIORef value)
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- lift $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else lift $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = (++ env) <$> mapM addBinding bindings
    addBinding (var, value) = (var,) <$> newIORef value
