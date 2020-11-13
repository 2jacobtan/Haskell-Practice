{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}


import Prelude hiding (and)
import Data.List (nub)
-- import Data.Function ((&))
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
  expr@(And _ _) -> expr
  
  Or (C False) f -> f
  Or (C True) _ -> C True
  Or f (C False) -> f
  Or _ (C True) -> C True
  expr@(Or _ _) -> expr

  Not (C False) -> C True
  Not (C True) -> C False
  expr@(Not _) -> expr
  
  expr@(C _) -> expr
  expr@(V _) -> expr

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

cnf :: Form -> Form
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

fv :: Form -> [String]
fv = nub . fvList
  where
    fvList = \case
      Or f1 f2 -> fvList f1 ++ fvList f2
      And f1 f2 -> fvList f1 ++ fvList f2
      Not f -> fvList f
      V v -> [v]
      C _ -> []

subst :: Form -> (String, Bool) -> Form
subst f0 (v0, b) = go f0
  where
    go = \case
      Or f1 f2 -> Or (go f1) (go f2)
      And f1 f2 -> And (go f1) (go f2)
      Not f -> Not (go f)
      V v
        | v == v0 -> C b
        | otherwise -> V v
      expr@(C _) -> expr

substAll :: Form -> [] (String, Bool) -> Form
substAll = foldl subst
-- substAll f varList
--   = map (flip subst) varList
--   & foldr (.) id
--   $ f
_ = substAll (V "a" `And` (Not (V "a") `Or` (Not (V "b")))) [("a", True), ("b", False)]

evalSubst :: Form -> [(String, Bool)] -> Bool
evalSubst f varList =
  case simplifyConst . substAll f $ varList of
    (C b) -> b
    _ -> error "rip"

_ = evalSubst (V "a" `And` (Not (V "a") `Or` (Not (V "b")))) [("a", True), ("b", False)] -- True
_ = evalSubst (V "a" `And` (Not (V "a") `Or` (Not (V "b")))) [("a", True), ("b", True)] -- False

models :: Form -> [(String, Bool)] -> [String] -> [[(String, Bool)]]
models f varPairs = \case
  v:vs -> models f ((v, True):varPairs) vs ++ models f ((v, False):varPairs) vs
  [] -> case evalSubst f varPairs of
    True -> [varPairs]
    False -> []

allModels :: Form -> [[(String, Bool)]]
allModels f = models f [] (fv f)

allModels' :: Form -> [[(String, Bool)]]
allModels' f = filter (evalSubst f)
  $ foldr (\x xs -> do
      x' <- x
      xs' <- xs
      return $ x':xs'
    ) [[]] [[(v,True), (v,False)] | v <- fv f]

-- x = foldr (\x xs -> do
--       x' <- x
--       xs' <- xs
--       return $ x':xs'
--     ) [[]] [[(v,True), (v,False)] | v <- fv (Or (V "a") (V "b"))]

unsatisfiable :: Form -> Bool
unsatisfiable f = case allModels' f of
  [] -> True
  _ -> False

valid :: Form -> Bool
valid = unsatisfiable . Not

a = valid (V "a" `Or` (Not (V "b")))
-- False
b = valid (V "a" `Or` (Not (V "a")))
-- True
c = unsatisfiable (V "a" `Or` (Not (V "b")))
-- False
d = unsatisfiable (V "a" `Or` (Not (V "a")))
-- False

main = mapM_ print [a,b,c,d]