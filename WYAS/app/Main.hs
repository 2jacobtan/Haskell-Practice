{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
-- import Control.Monad (liftM)
import Parsing (readExpr)

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
