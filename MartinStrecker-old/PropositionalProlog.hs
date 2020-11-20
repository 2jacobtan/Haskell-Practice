{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import PropositionalFormulas

data Rule = Rl {head :: String, body :: [String]}

newtype Goal = Gl {unGl :: [String]}

data Prog = Pr {rules :: [Rule], goal :: Goal}

mortalSocrates :: Prog
mortalSocrates =
  Pr
    [ Rl "h" [],
      Rl "m" ["h"]
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

