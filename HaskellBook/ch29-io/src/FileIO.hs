{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module FileIO (readAndWrite) where

import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (throwE, runExceptT)
import System.IO
import System.Environment (getArgs)

timeLimit = 4 -- seconds

readAndWrite :: IO (Either String Int)
readAndWrite = runExceptT $ do

  -- check args
  args <- lift getArgs
  case args of
    ["-d"] -> lift $ putStrLn "-d arg detected."
    ["-e"] -> lift $ putStrLn "-e arg detected."
    _ -> throwE "Invalid args. Use -d or -e." 
  
  -- time limit
  inTimeLimit <- lift $ hWaitForInput stdin (timeLimit * 1000)
  if inTimeLimit then return ()
    else throwE $ "Exceeded time limit of " ++ show timeLimit ++ " seconds."
  
  -- read and write
  input <- lift $ hGetLine stdin
  lift $ hPutStrLn stdout $ "Input was: " ++ input
  return 0