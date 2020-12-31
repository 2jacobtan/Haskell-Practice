{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Evaluation where

import Control.Monad.Except ( MonadError(throwError, catchError) )
import Control.Monad.Trans (MonadTrans(lift))
import Data.Function ((&))
import Data.Maybe (isNothing)

import Types
    ( showVal,
      Env,
      LispError(NumArgs, BadSpecialForm, TypeMismatch),
      LispVal(List, Number, String, Atom, DottedList, Bool,
              PrimitiveFunc, Func, closure, body, vararg, params),
      ThrowsError )
import VarsAndAssignment
    (nullEnv,  IOThrowsError, liftThrows, getVar, setVar, defineVar, bindVars )
import Control.Monad (join)


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id  -- from: Adding Variables and Assignment
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval env pred
        case result of
             Bool False -> eval env alt
             Bool True  -> eval env conseq
             _ -> throwError $ TypeMismatch "Bool" pred
eval env (List [Atom "set!", Atom var, form]) =
   eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
   eval env form >>= defineVar env var
   
-- from: Defining Scheme Functions
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List (function : args)) = join $ apply <$> eval env function <*> mapM (eval env) args
-- eval env (List (function : args)) = do
--   func <- eval env function
--   argVals <- mapM (eval env) args
--   apply func argVals

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


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
   ("string>=?", strBoolBinop (>=)),
   ("car", car),
   ("cdr", cdr),
   ("cons", cons),
   ("eq?", eqv),
   ("eqv?", eqv),
   ("equal?", equal)   
   ]              

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _            []  = throwError $ NumArgs 2 []
numericBinop _  singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed & head
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
                             else do   left <- unpacker $ args & head
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

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]             = equalList eqv arg1 arg2
-- eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
--                                                              (all eqvPair $ zip arg1 arg2)
--      where eqvPair (x1, x2) = case eqv [x1, x2] of
--                                 Left _ -> False
--                                 Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [List xs, List ys] = equalList equal xs ys
equal [DottedList xs x, DottedList ys y] = equalList equal (x:xs) (y:ys)
equal [arg1, arg2] = do
      primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2) 
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

equalList eqv'equal xs ys = return $ Bool $
   (length xs == length ys)
   && all equalPair (zip xs ys)
   where
      equalPair (x, y) = case eqv'equal [x, y] of
         Left _ -> False
         Right (Bool val) -> val
         Right _ -> error "only accepts (Bool val)"


-- from: Defining Scheme Functions

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc vararg env params body = return $ Func (map showVal params) vararg body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal

apply (PrimitiveFunc func) args = liftThrows $ func args

apply Func {..} args =
  if num params /= num args && isNothing vararg
    then throwError $ NumArgs (num params) args
    else lift (bindVars closure $ zip params args) >>= bindVarArgs vararg >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = last <$> mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> lift $ bindVars env [(argName, List remainingArgs)]
      Nothing -> return env

apply _ _ = error "apply: only for PrimitiveFunc / Func"
