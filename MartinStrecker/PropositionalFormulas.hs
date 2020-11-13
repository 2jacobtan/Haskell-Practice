data Form
  = C Bool
  | Not Form
  | And Form Form
  | Or Form Form
  | V String

_ = ((V "a" `And` (Not (V "b"))) `Or` ((V "c") `Or` (C False)))

_ = Or (And (V "a") (Not (V "b"))) (Or (V "c") (C False))

