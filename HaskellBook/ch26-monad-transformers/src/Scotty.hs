{-# LANGUAGE OverloadedStrings #-}

module Scotty where

-- import Data.Monoid (mconcat)

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Control.Monad.Trans.State.Strict (StateT (StateT))
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Web.Scotty
import Web.Scotty.Internal.Types (ActionT (ActionT))

param' :: Parsable a => Text -> MaybeT ActionM a
param' k = MaybeT $
  rescue (Just <$> param k)
    (const (return Nothing))

type Reco =
  (Integer, Integer, Integer, Integer)

main = scotty 3000 $ do
  get "/:word" $ do
    beam' <- runMaybeT $ param' "word"
    let beam = fromMaybe "" beam'
    
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

    -- Temporary extension of structure
    reco <- runMaybeT $ do
      a <- param' "1"
      liftIO $ print a
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift . lift) $ print b
      return ((a, b, c, d) :: Reco)
    liftIO $ print reco    
    
    html $
      mconcat
        [ "<h1>Scotty, ",
          beam,
          " me up!</h1>"
        ]

liftReaderT :: m a -> ReaderT r m a
liftReaderT = ReaderT . const