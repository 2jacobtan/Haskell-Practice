-- random example by Andreas, on Slack
-- https://smucclaw.slack.com/archives/C019BR4LX9V/p1605690172323800?thread_ts=1605195773.294900&cid=C019BR4LX9V

{-# LANGUAGE TypeApplications, ScopedTypeVariables, ExplicitForAll #-}

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

-- Nada :: forall a. Optional a
-- Only :: forall a. a -> Optional a
-- First' :: forall a. Optional a -> First' a
-- Functions versions of the constructors



nada' :: forall a. Optional a
nada' = Nada @a
onlyOne :: First' Integer
onlyOne = First' @Integer (Only @Integer (1 :: Integer))
onlyOneGeneric :: forall a. Num a => First' a
onlyOneGeneric = First' @a (Only @a (1 :: a))
-- In more explicit pseudo-haskell:
-- onlyOneGeneric @a @dict_num_a = First' @a (Only @a (1 @a @dict_num_a))
-- This function takes two implicit parameters, one for the type and another for the
-- dictionary that describes how the type is an instance of Num.
nada :: forall a. First' a
nada = First' @a (Nada @a)
test6 :: First' Integer
test6 = mappend @(First' Integer) onlyOne (nada @Integer)
test7 :: forall a. First' a
test7 = mappend @(First' a) (nada @a) (nada @a)
test8 :: First' Integer
test8 = mappend @(First' Integer) onlyOne onlyTwo