-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}

-- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/type_families.html#injective-type-families
type family Id a = result | result -> a where
type family F a b c = d | d -> a c b
type family G (a :: k) b c = foo | foo -> k b where
