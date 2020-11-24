module MVar where

import Control.Concurrent

main :: IO ()
main = do
  mv <- newEmptyMVar :: IO (MVar Int)
  forkIO $ do
    takeMVar mv >>= print
  putMVar mv (42 :: Int)
