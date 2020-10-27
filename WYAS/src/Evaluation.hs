{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Evaluation where

import Parsing
import Control.Monad.Except

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             _          -> eval conseq
eval (List (Atom func : args)) = apply func =<< mapM eval args
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
   ("+", numericBinop (+)),
   ("-", numericBinop (-)),
   ("*", numericBinop (*)),
   ("/", numericBinop div),
   ("mod", numericBinop mod),
   ("quotient", numericBinop quot),
   ("remainder", numericBinop rem),
   ("symbol?" , unaryOp symbolp) ,
   ("string?" , unaryOp stringp) ,
   ("number?" , unaryOp numberp) ,
   ("bool?", unaryOp boolp) ,
   ("list?" , unaryOp listp),
   ("symbol->string", unaryOp symbol2string),
   ("string->symbol", unaryOp string2symbol),
   ("=", numBoolBinop (==)),
   ("<", numBoolBinop (<)),
   (">", numBoolBinop (>)),
   ("/=", numBoolBinop (/=)),
   (">=", numBoolBinop (>=)),
   ("<=", numBoolBinop (<=)),
   ("&&", boolBoolBinop (&&)),
   ("||", boolBoolBinop (||)),
   ("string=?", strBoolBinop (==)),
   ("string<?", strBoolBinop (<)),
   ("string>?", strBoolBinop (>)),
   ("string<=?", strBoolBinop (<=)),
   ("string>=?", strBoolBinop (>=))
   ]              

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _            []  = throwError $ NumArgs 2 []
numericBinop _  singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = return . Number . foldl1 op =<< mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v
unaryOp _ []  = throwError $ NumArgs 1 []
unaryOp _ manyVal = throwError $ NumArgs 1 manyVal

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool False
listp   _          = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""


-- Evaluation, Part 2
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do   left <- unpacker $ args !! 0
                                       right <- unpacker $ args !! 1
                                       return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList
