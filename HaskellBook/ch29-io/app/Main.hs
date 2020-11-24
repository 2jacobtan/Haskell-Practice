module Main where

import FileIO
import System.IO

main :: IO ()
main = do
  result <- readAndWrite
  case result of
    Left e -> hPutStrLn stderr e
    Right n -> print n
