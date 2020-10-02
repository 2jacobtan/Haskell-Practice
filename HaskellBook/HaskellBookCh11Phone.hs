{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
import Data.Char (isUpper, toLower, toUpper)
import Data.List (elemIndex, foldl')
import Data.Maybe
import qualified Debug.Trace

-- K is a normal key, KS is the * (star) key.
data KeyPress = K Int  | KA deriving Show
data KeyResult = R String | Caps deriving Show

type DaPhone = [(KeyPress, KeyResult)]
daPhone :: DaPhone
daPhone = [
    (K 1 , R "1"),
    (K 2 , R "abc2"),
    (K 3 , R "def3"),
    (K 4 , R "ghi4"),
    (K 5 , R "jkl5"),
    (K 6 , R "mno6"),
    (K 7 , R "pqrs7"),
    (K 8 , R "tuv8"),
    (K 9 , R "wxyz9"),
    (K 0 , R "+ _0"),
    (K 12 , R ".,"),
    (KA , Caps)
    ]

type PressCount = Int
reverseTaps :: Char -> [(KeyPress, PressCount)]
reverseTaps c =
    prepend [key'count daPhone]
    where
        prepend = if isUpper c then \xs -> (KA, 1) : xs else id
        ch = toLower c
        key'count ((key , R chars):others) = case elemIndex ch chars of
            Just n -> (key, n+1)
            Nothing -> key'count others 
        key'count _ = error "rip"
encodeConvo string = reverseTaps <$> string

makeTaps :: [(KeyPress, PressCount)] -> Char
makeTaps = \case
    [(KA,1),(key, count)] -> toUpper $ getChar (key, count)
    [(key, count)] -> getChar (key, count)
    _ -> error "rip"
    where
        getChar (key, count) =
            go key count daPhone
            where
                go (K keyNum) c ((K keyNum', R string):others)
                    | keyNum == keyNum' =  string !! (c-1)
                    | otherwise = go (K keyNum) c others
                go _ _ _ = error "rip"

decodeConvo xs = makeTaps <$> xs

digitsPressedCount message = countDigits $ encodeConvo message
    where
        countDigits encoded = sum $ getCount <$> encoded
        getCount [(KA,_),(key,count)] = count
        getCount [(key,count)] = count
        getCount _ = error "rip"

print' :: Show a => a -> IO ()
print' = putStrLn . show -- needs type annotation to please monomorphism restriction
print'' a = putStrLn . show $ a
plus a b = a + b
plus' a b = Debug.Trace.trace "plus' is run once" $ a + b
plus'' a b = Debug.Trace.trace (show a ++ " plus " ++ show b) $ a + b
main = do 
    print "Hello World!"
    putStrLn ""
    print $ reverseTaps 'e'
    print $ makeTaps [(KA, 1),(K 3,2)]
    print $ makeTaps . reverseTaps $ 'F'
    putStrLn ""
    print $ encodeConvo "Hello, world."
    print $ decodeConvo . encodeConvo $ "Hello, world."
    putStrLn ""
    print $ digitsPressedCount "Hello, world."
    print $ digitsPressedCount "hELLO, WORLD."
