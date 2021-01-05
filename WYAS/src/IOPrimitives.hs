module IOPrimitives where

import System.IO
import Control.Monad.Trans (MonadTrans(lift))
import Data.Functor (($>))

import Evaluation (load, apply)
import Parsing (readExpr, readExprList)
import Types
import VarsAndAssignment (liftThrows)
import Control.Monad.Except (ExceptT(ExceptT))

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [
  ("apply", applyProc),
  ("open-input-file", makePort ReadMode),
  ("open-output-file", makePort WriteMode),
  ("close-input-port", closePort),
  ("close-output-port", closePort),
  ("read", readProc),
  ("write", writeProc),
  ("read-contents", readContents),
  ("read-all", readAll)
  ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = lift $ Port <$> openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = lift $ hClose port $> Bool True
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = ExceptT $ readExpr <$> hGetLine port
-- readProc [Port port] = liftThrows . readExpr =<< lift (hGetLine port)

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = lift $ hPrint port obj $> Bool True

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = lift $ String <$> readFile filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename
