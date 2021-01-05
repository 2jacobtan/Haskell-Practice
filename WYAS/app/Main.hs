module Main (main) where

import Control.Monad ((<=<), liftM)
import System.Environment ( getArgs )

import Evaluation (eval)
import Parsing (extractValue, readExpr, trapError)
import Repl ( runRepl, runOne )

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runOne args

-- main = putStrLn . extractValue . trapError . fmap show . (eval <=< (readExpr . head)) =<< getArgs

-- main = do
--      args <- getArgs
--      evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
--      putStrLn $ extractValue $ trapError evaled
