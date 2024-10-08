{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception
import System.Environment (getArgs)

willIFail ::
  Integer ->
  IO (Either ArithException ())
willIFail denom =
  try $ print $ div 5 denom

onlyReportError ::
  Show e =>
  IO (Either e a) ->
  IO ()
onlyReportError action = do
  result <- action
  case result of
    Left e -> print e
    Right _ -> return ()

testDiv :: String -> IO ()
testDiv d =
  -- onlyReportError $ willIFail (read d)
  -- onlyReportError @ArithException $ try $ print $ div 5 (read d)
  onlyReportError @SomeException $ try $ print $ div 5 (read d)

main :: IO ()
main = do
  args <- getArgs
  mapM_ testDiv args