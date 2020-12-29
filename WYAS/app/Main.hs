
module Main (main) where

import Control.Monad ((<=<), liftM)
import Evaluation (eval)
import Parsing (extractValue, readExpr, trapError)
import System.Environment

main :: IO ()
main = putStrLn . extractValue . trapError . fmap show . (eval <=< (readExpr . head)) =<< getArgs

-- main = do
--      args <- getArgs
--      evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
--      putStrLn $ extractValue $ trapError evaled
