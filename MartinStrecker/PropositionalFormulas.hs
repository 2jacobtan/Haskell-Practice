{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}


import Prelude hiding (and)
-- import Data.Maybe (fromMaybe)
-- import Control.Applicative (Alternative((<|>)))

data Form
  = C Bool
  | Not Form
  | And Form Form
  | Or Form Form
  | V String
  deriving (Eq, Show, Read)

_ = ((V "a" `And` (Not (V "b"))) `Or` ((V "c") `Or` (C False)))

_ = Or (And (V "a") (Not (V "b"))) (Or (V "c") (C False))

-- removeConst :: Form -> Form
-- removeConst expr@(And _ _) = and expr

-- and expr@(And left right) = fromMaybe expr (and' left right <|> and' right left)
-- and' (C True) f = Just f
-- and' (C False) _ = Just $ C False
-- and' _ _ = Nothing

-- or expr@(Or left right) = fromMaybe expr (or' left right <|> or' right left)
-- or' (C True) _ = Just $ C True
-- or' (C False) f = Just $ f
-- or' _ _ = Nothing

removeConst :: Form -> Form
removeConst = \case
  And (C False) _ -> C False
  And (C True) f -> f
  And _ (C False) -> C False
  And f (C True) -> f
  expr@(And _ _) -> expr -- optional
  
  Or (C False) f -> f
  Or (C True) _ -> C True
  Or f (C False) -> f
  Or _ (C True) -> C True
  expr@(Or _ _) -> expr -- optional

  Not (C False) -> C True
  Not (C True) -> C False
  expr@(Not _) -> expr -- optional

  expr -> expr

simplifyConst :: Form -> Form
simplifyConst = \case
  And f1 f2 -> removeConst (And (simplifyConst f1) (simplifyConst f2))
  Or f1 f2 -> removeConst (Or (simplifyConst f1) (simplifyConst f2))
  Not f -> removeConst (Not (simplifyConst f))
  expr@(C _) -> expr
  expr@(V _) -> expr

nnf :: Form -> Form
nnf = \case
  (Not (Not f)) -> nnf f
  (Not (And f1 f2)) -> Or (nnf (Not f1)) (nnf (Not f2))
  (Not (Or f1 f2)) -> And (nnf (Not f1)) (nnf (Not f2))
  (Not f) -> Not (nnf f)
  And f1 f2 -> And (nnf f1) (nnf f2)
  Or f1 f2 -> Or (nnf f1) (nnf f2)
  expr@(V _) -> expr
  expr@(C _) -> expr

cnf = \case
  -- apply rule
  Or p (And q r) -> cnf (Or p q) `And` cnf (Or p r)
  Or (And q r) p -> cnf (Or q p) `And` cnf (Or r p)
  -- other cases
  Or f1 f2 -> cnf f1 `Or` cnf f2
  And f1 f2 -> cnf f1 `And` cnf f2
  Not f -> cnf f
  expr@(V _) -> expr
  expr@(C _) -> expr

