import Data.Char (isUpper, toLower)
import Data.List (elemIndex)

type Digit = Char -- any of "1234567890*#"
type Letters = String -- "abc", "+ ", ".," etc
type Presses = Int
newtype DaPhone = DaPhone [(Digit, Letters)] deriving Show
aPhone :: DaPhone
aPhone = DaPhone [
  ('1', "1"),
  ('2', "abc2"),
  ('3', "def3"),
  ('4', "ghi4"),
  ('5', "jkl5"),
  ('6', "mno6"),
  ('7', "pqrs7"),
  ('8', "tuv8"),
  ('9', "wxyz9"),
  ('*', "^*"),
  ('0', "+ 0"),
  ('#', ".,#")
  ]

-- modified by JT2
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone tupList) char =
  foldr rf [] tupList
    where
      (digit, letters) `rf` acc = case elemIndex char letters of
        Nothing -> acc
        Just charPos ->
          [('*', 1) | isUpper char]
          ++ [(digit, charPos)]
getPosition :: Char -> String -> Int
getPosition char str = go (toLower char) str 0
  where
    go _ [] _ = 0
    go c (x:xs) count
         | c == x = count + 1
         | otherwise = go c xs (count + 1)
cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead = concatMap . reverseTaps
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) acc -> p + acc) 0