{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module MatrixQQ where

import Data.Function
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Read (readMaybe)

matrix :: QuasiQuoter
matrix =
  QuasiQuoter
    { quoteExp = matrixQuoteExp,
      quotePat = undefined,
      quoteDec = undefined,
      quoteType = undefined
    }

matrixQuoteExp :: String -> Q Exp
matrixQuoteExp str =
  case parseMatrix @Int str of
    Left e -> fail e
    Right mat -> liftData mat

parseMatrix :: forall a. (Num a, Read a) => String -> Either String [[a]]
parseMatrix (lines -> fmap words -> filter (not . null) -> mat) =
  mat
    & traverse
      ( traverse
          (\word -> maybe (Left ("Invalid int: " ++ word)) Right (readMaybe @a word))
      )