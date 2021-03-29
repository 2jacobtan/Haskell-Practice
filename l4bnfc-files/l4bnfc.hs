import System.IO
import Control.DeepSeq (NFData(rnf))
import GHC.IO.Encoding (getLocaleEncoding)


readFile' :: FilePath -> IO String
readFile' path' = do
  inFile   <- openFile path' ReadMode
  contents <- hGetContents inFile
  rnf contents `seq` hClose inFile
  return contents
  
main = do
  -- putStr =<< readFile' "l4.bnfc"
  print =<< getLocaleEncoding

