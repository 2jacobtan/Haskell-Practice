import Control.Monad.Trans.Reader
import Data.Functor.Identity (Identity)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Text.Pretty.Simple (pPrint)
import Control.Monad.Trans.State (runStateT, put, get, StateT)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad (guard)

p x = print x
ps = putStrLn
p_ = putStrLn ""

main = do
  ps "rDec"
  p $ runReader rDec 1
  p $ fmap (runReader rDec) [1..10]
  p_
  ps "rShow"
  p $ runReader rShow 1
  p $ fmap (runReader rShow) [1..10]
  p_
  ps "rPrintAndInc"
  runReaderT rPrintAndInc 1 >>= pPrint
  traverse (runReaderT rPrintAndInc) [1..10] >>= pPrint
  p_
  ps "sPrintIncAccum"
  runStateT sPrintIncAccum 10 >>= print
  mapM (runStateT sPrintIncAccum) [1..3] >>= print

rDec = rDec2
-- | q1 
rDec1 :: ReaderT Integer Identity Integer
rDec1 = reader $ do
  a <- subtract 1
  return $ a

-- | q2
rDec2 :: ReaderT Integer Identity Integer
rDec2 = reader (subtract 1)

-- | q3 q4
rShow :: Show a => ReaderT a Identity String
rShow = reader show

-- | q5
rPrintAndInc ::
  (Num a, Show a) =>
  ReaderT a IO a
rPrintAndInc = do
  ReaderT $ putStrLn . ("Hi: " ++) . show
  result <- ReaderT $ return . (+1)
  return result

-- | Doesn't work because Reader's bind (>>=) doesn't run the IO monad produced by the r -> IO () function in the first line. It simply throws it away.
-- rPrintAndInc = ReaderT $ do
--   putStrLn . ("Hi: " ++) . show
--   result <- return . (+1)
--   return result

-- | Also doesn't work
-- rPrintAndIncX :: Show a => ReaderT a IO ()
-- rPrintAndIncX = ReaderT $ do
--   \x -> seq (putStrLn . ("Hi: " ++) . show $ x) (return () :: IO ())
--   return . return $ ()

-- | q6
sPrintIncAccum ::
  (Num a, Show a) =>
  StateT a IO String
sPrintIncAccum = do
  input <- get
  let stringInput = show input
  lift $ putStrLn $ "Hi: " ++ stringInput
  put $ input + 1
  return stringInput

-- | Fix the code
isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn
        ("Good, was very excite: " ++ e)