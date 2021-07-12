--- 2021-07-12 -- refactoring

{-# LANGUAGE LambdaCase #-}

{-
termItem :: String -> Item -> Item
-- Leaf elided
termItem ter (Any pre children) =
  Any
    pre
    (init children ++ [termItem ter (last children)])
termItem ter (Any (PrePost pre post) children) =
  Any
    (PrePost pre (post ++ ter))
    children
termItem ter (All pre children) =
  All
    pre
    (init children ++ [termItem ter (last children)])
termItem ter (All (PrePost pre post) children) =
  All
    (PrePost pre (post ++ ter))
    children
-}

-- refactoring of above

termItem :: String -> Item -> Item
termItem ter = \case
  (Leaf _) -> error "elided"
  (Any label items) -> go Any label items
  (All label items) -> go All label items
  where
    go c (PrePost pre post) items = c (PrePost pre (post ++ ter)) items
    go c pre@(Pre _) items = c pre (init items ++ [termItem ter (last items)])

data Item =
    Leaf String
  | Any Label [Item]
  | All Label [Item]
data Label =
    Pre String
  | PrePost String String

----------------------------------------------------------------