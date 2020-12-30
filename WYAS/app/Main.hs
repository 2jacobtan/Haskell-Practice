
module Main (main) where

import Control.Monad ((<=<), liftM)
import System.Environment

import Evaluation (eval)
import Parsing (extractValue, readExpr, trapError)
import Repl

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ head args
    _-> putStrLn "Program takes only 0 or 1 argument"


-- main = putStrLn . extractValue . trapError . fmap show . (eval <=< (readExpr . head)) =<< getArgs

-- main = do
--      args <- getArgs
--      evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
--      putStrLn $ extractValue $ trapError evaled
