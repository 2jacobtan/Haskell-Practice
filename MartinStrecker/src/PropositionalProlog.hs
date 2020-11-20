{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module PropositionalProlog where

import PropositionalFormulas hiding (main)
import Prelude hiding (head)

data Rule = Rl {head :: String, body :: [String]}
  deriving Show

newtype Goal = Gl {unGl :: [String]}

data Prog = Pr {rules :: [Rule], goal :: Goal}

mortalSocrates :: Prog
mortalSocrates =
  Pr
    [ Rl "h" [],
      Rl "m" ["h"]
    ]
    (Gl ["m"])

immortalSocrates :: Prog
immortalSocrates =
  Pr
    [ Rl "h" [],
      Rl "h" ["m"]
    ]
    (Gl ["m"])

abcdProg :: Prog
abcdProg =
  Pr
    [ Rl "a" [],
      Rl "d" ["b", "c"],
      Rl "d" ["a", "c"],
      Rl "c" ["a"]
    ]
    (Gl ["d", "c"])

implies :: Form -> Form -> Form
implies a b = Not a `Or` b

conj :: [Form] -> Form
conj xs = foldr And (C True) xs

vToForm :: String -> Form
vToForm str = V str

vsToForm :: [String] -> [Form]
vsToForm strList = map V strList

ruleToForm :: Rule -> Form
ruleToForm (Rl {head, body}) = conj (vsToForm $ body) `implies` vToForm (head)

goalToForm :: Goal -> [Form]
goalToForm (Gl g) = vsToForm g

progToForm :: Prog -> Form
progToForm (Pr {rules, goal}) = conj (map ruleToForm rules) `implies` conj (goalToForm goal)

main :: IO ()
main = do
  print $ valid . progToForm $ mortalSocrates
  print $ valid . progToForm $ immortalSocrates
  print $ allModels . Not. progToForm $ immortalSocrates
  
  putStrLn "\nany: divisible by 3"
  let divisible3 x = x `mod` 3 == 0
      numbers = [1, 4, 5, 6]
  print $ any divisible3 numbers
  print $ filter divisible3 numbers

  putStrLn "\nall: divisible by 3"
  print $ all divisible3 numbers

  putStrLn "\nExercise 18"
  print $ map (^2) . filter (<0) $ [1, -3, 4, -5]
  print $ [x^2 | x <- [1, -3, 4, -5] , x < 0]

  print $ [r | r <- rules abcdProg, head r == "d" ]

