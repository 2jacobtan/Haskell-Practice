{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans
import Control.Monad.Trans.Except (runExceptT)
import Data.Function ((&))
import Web.Scotty.Internal.Types (ActionT(runAM))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.State.Strict (StateT(runStateT))
data Config =
  Config {
  -- that's one, one click!
  -- two... two clicks!
  -- Three BEAUTIFUL clicks! ah ah ahhhh
  counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)
type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoomp ::
  Text ->
  M.Map Text Integer ->
  (M.Map Text Integer, Integer)
bumpBoomp k m =
  let newValue = 1 + fromMaybe 0 (M.lookup k m)
  in
    ( M.insert k newValue m,
      newValue
    )

app :: Scotty ()
app =
  get "/:key" $ do
    (unprefixed :: Text) <- param "key"
    prefixText <- lift $ asks prefix
    let key' = mappend prefixText unprefixed
    newInteger <- lift $ do
      refCount <- asks counts
      countsMap <- lift $ readIORef refCount
      let (newCountsMap, newCount) = bumpBoomp (key' :: Text) countsMap
      lift $ writeIORef refCount newCountsMap
      return newCount
    return () -- :: ActionT Text (ReaderT Config IO) ()

    let actionT_IO = html $
          mconcat
            [ "<h1>Success! Count was: ",
              TL.pack $ show newInteger,
              "</h1>"
            ]
    runAM actionT_IO & runExceptT & runReaderT & fmap runStateT & _

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR = ($ config) . runReaderT
  scottyT 3000 runR app