import Data.Monoid
import Data.Functor.Constant
xs = [Just 1, Just 2, Nothing]
_ = fmap product xs


data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj]
  -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn
  :: Query
  -> IO (Either Err [(SomeObj, IoOnlyObj)])

pipelineFn = id
  . (=<<) (traverse makeIoOnlyObj . traverse decodeFn)
  . fetchFn


xs1 = map Sum [1 .. 4]
txs1 = traverse (Constant . (*2)) xs1

