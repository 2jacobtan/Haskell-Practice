{-# LANGUAGE OverloadedStrings #-}

module DataTree2 where

import Data.Tree
import Data.Text ( unpack, Text )
import Data.String (IsString)

-- “I may order (strawberry U chocolate) (ice cream U cake) on (Saturdays U Sundays)”


example2 :: Tree [Text]
example2 =
  Node ["MAY_"]
    [ Node ["I"] [],
      Node ["ON_"]
        [ Node ["ORDER_"]
            [ Node
                [ "chocolate",
                  "strawberry",
                  and' ["peanut butter", "jelly"]] []
            ],
          Node ["Saturdays"] []
        ]
    ]

and' :: [Text] -> Text
and' = foldr1 (\x y -> x <> " and " <> y)

example2_ :: [Tree Text]
example2_ = sequence example2

linearise :: (Show a, Semigroup a, IsString a, Eq a) => Tree a -> a
linearise (Node "MAY_" [subj, verbPhrase])
  = linearise subj <> " may " <> linearise verbPhrase
linearise (Node "ON_" [nounPhrase, situation])
  = linearise nounPhrase <> " on " <> linearise situation
linearise (Node "ORDER_" [object])
  = "order " <> linearise object
linearise (Node "ADJ_" [adjPhrase, nounPhrase])
  = linearise adjPhrase <> " " <> linearise nounPhrase
linearise (Node atom [])
  = atom
linearise (Node x y)
  = error $ "unrecognised non-atom: Node " <> show x <> " " <> show y


main :: IO ()
main = do
  mapM_ (putStrLn . unpack . linearise) example2_
  mapM_ (putStrLn . drawTree . fmap unpack) example2_

{-



-}