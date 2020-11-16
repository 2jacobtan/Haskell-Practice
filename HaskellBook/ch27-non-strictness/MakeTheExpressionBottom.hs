{-# LANGUAGE BangPatterns #-}

-- Make the expression bottom

x = undefined
-- y = "blah"
y = seq x "blah"
main = do
  print (snd (x, y))