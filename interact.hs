{-# language TypeApplications #-}

f :: String -> String
f = show . (*100) . read @Int

main :: IO ()
main = do
  putStrLn "Hello." 
  interact $ unlines . map f . lines
  -- interact id
