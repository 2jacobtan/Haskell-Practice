{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
-- import Control.Monad (liftM)
import Parsing (readExpr)
import Evaluation (eval)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head