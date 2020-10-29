import Test.Hspec
import Control.Monad (liftM)
import Evaluation (eval)
import Parsing (extractValue, readExpr, trapError)
parse = extractValue . trapError . liftM show . (eval =<<) . readExpr

i x y = it "" $ parse x `shouldBe` y

main :: IO ()
main = hspec $ do
  describe "Evaluation, Part 2" $ do
    i "(cdr '(a simple test))" $ "(simple test) -fail"
    i "(car (cdr '(a simple test)))" $ "simple -fail"

