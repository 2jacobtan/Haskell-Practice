{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data.List

data Form =
    C Bool 
  | V String 
  | Not Form
  | Form `Or` Form
  | Form `And` Form
  deriving (Eq, Ord, Show, Read)

-- Semantic Equivalence 
removeConst :: Form -> Form
removeConst (f `And` (C True)) = removeConst f
removeConst (_ `And` (C False)) = C False
removeConst ((C True) `And` f) = removeConst f
removeConst ((C False) `And` _) = C False
removeConst (_ `Or` (C True)) = C True
removeConst (f `Or` (C False)) = removeConst f
removeConst ((C True) `Or` _) = C True
removeConst ((C False) `Or` f) = removeConst f
removeConst (Not (C True)) = C False
removeConst (Not (C False)) = C True
removeConst f = f


simplifyConst :: Form -> Form
simplifyConst (Not f) = 
  removeConst (Not (removeConst f)) 
simplifyConst (f1 `And` f2) = 
  removeConst ((removeConst f1) `And` (removeConst f2))
simplifyConst (f1 `Or` f2) =
  removeConst ((removeConst f1) `Or` (removeConst f2))

-- Negation Normal Form
nnf :: Form -> Form 
nnf (Not (Not f)) = f
nnf (Not (f1 `Or` f2)) = (Not f1) `And` (Not f2)
nnf (Not (f1 `And` f2)) = (Not f1) `Or` (Not f2)

-- Conjunctive Normal Form
distribOr :: Form -> Form -> Form
distribOr (a `And` b) (c `And` d) = 
  (distribOr (a `And` b) c) `And` (distribOr (a `And` b) d)
distribOr (a `And` b) c = (a `Or` c) `And` (b `Or` c)
distribOr a (b `And` c) = (a `Or` b) `And` (a `Or` c)
distribOr a b = a `Or` b


cnf :: Form -> Form
cnf (f1 `Or` (f2 `And` f3)) = distribOr f1 (f2 `And` f3)
cnf ((f1 `And` f2) `Or` f3) = distribOr (f1 `And` f2) f3



-- Auxillary Functions

fvList :: Form -> [String]
fvList (f1 `Or` f2) = nub ((++) (fvList f1) (fvList f2)) 
fvList (f1 `And` f2) = nub ((++) (fvList f1) (fvList f2)) 
fvList (Not f) = fvList f
fvList (V x) = [x]

subst :: Form -> (String, Bool) -> Form
subst (f1 `And` f2) s = (subst f1 s) `And` (subst f2 s)
subst (f1 `Or` f2) s = (subst f1 s) `Or` (subst f2 s)
subst (Not f) s = Not (subst f s)
subst f@(V v) (x,y) = if v == x then C y else f
subst f _ = f

substAll :: Form -> [(String, Bool)] -> Form
substAll expr subList = foldl (subst) expr subList 

evalSubst :: Form -> [(String, Bool)] -> Bool
evalSubst expr subList 
  | x == C False = False
  | x == C True = True
  | otherwise = evalSubst x subList
    where x = simplifyConst (substAll expr subList)


-- Model Finding
models :: Form -> [(String, Bool)] -> [String] -> [[(String, Bool)]]
models f vl [] = if evalSubst f vl == True then [vl] else []
models f vl (vn:vns) = 
  (++) (models f ((vn, True):vl) vns) (models f ((vn, False): vl) vns)


allModels :: Form -> [[(String, Bool)]]
allModels f = models f [] $ fvList f

unsatisfiable :: Form -> Bool
unsatisfiable f = if allModels f == [] then True else False

valid :: Form -> Bool
valid f = if unsatisfiable (Not f) then True else False


expr = (V "a" `Or` (Not (V "a")))

