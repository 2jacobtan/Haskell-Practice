{-# LANGUAGE OverloadedStrings #-}

module Scotty where

-- import Data.Monoid (mconcat)

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Control.Monad.Trans.State.Strict (StateT (StateT))
import Web.Scotty
import Web.Scotty.Internal.Types (ActionT (ActionT))
import Control.Monad.IO.Class (MonadIO(liftIO))

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    lift $ putStrLn "hello"
    ActionT . lift . lift . lift $ putStrLn "lift 3"
    ActionT
      . (ExceptT . fmap Right)
      . liftReaderT
      -- . lift
      . (\m -> StateT (\s -> m >>= \a -> return (a, s)))
      -- . (\m -> StateT (
      --     \s -> do
      --       a <- m
      --       return (a, s)
      --     )
      --   )
      $ putStrLn "lift ExceptT + ReaderT"
    liftIO $ putStrLn "liftIO"
    html $
      mconcat
        [ "<h1>Scotty, ",
          beam,
          " me up!</h1>"
        ]

liftReaderT :: m a -> ReaderT r m a
liftReaderT = ReaderT . const