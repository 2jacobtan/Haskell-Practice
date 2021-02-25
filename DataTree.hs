{-# LANGUAGE OverloadedStrings #-}

module DataTree where

import Data.Tree
import Data.Text
import Data.String (IsString)

-- “I may order (strawberry U chocolate) (ice cream U cake) on (Saturdays U Sundays)”


example :: Tree [Text]
example =
  Node ["MAY_"]
    [ Node ["I"] [],
      Node ["ON_"]
        [ Node ["ORDER_"]
            [ Node ["ADJ_"]
                [ Node ["strawberry","chocolate"] [],
                  Node ["ice cream", "cake"] []
                ]
            ],
          Node ["Saturdays", "Sundays"] []
        ]
    ]

example_ :: [Tree Text]
example_ = sequence example

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
  mapM_ (putStrLn . unpack . linearise) example_
  mapM_ (putStrLn . drawTree . fmap unpack) example_

{-

[1 of 1] Compiling DataTree         ( /home/jt2/repos/Haskell-Practice/DataTree.hs, interpreted )
Ok, one module loaded.
*DataTree PPrint> main
I may order strawberry ice cream on Saturdays
I may order strawberry ice cream on Sundays
I may order strawberry cake on Saturdays
I may order strawberry cake on Sundays
I may order chocolate ice cream on Saturdays
I may order chocolate ice cream on Sundays
I may order chocolate cake on Saturdays
I may order chocolate cake on Sundays
MAY_
|
+- I
|
`- ON_
   |
   +- ORDER_
   |  |
   |  `- ADJ_
   |     |
   |     +- strawberry
   |     |
   |     `- ice cream
   |
   `- Saturdays
MAY_
|
+- I
|
`- ON_
   |
   +- ORDER_
   |  |
   |  `- ADJ_
   |     |
   |     +- strawberry
   |     |
   |     `- ice cream
   |
   `- Sundays
MAY_
|
+- I
|
`- ON_
   |
   +- ORDER_
   |  |
   |  `- ADJ_
   |     |
   |     +- strawberry
   |     |
   |     `- cake
   |
   `- Saturdays
MAY_
|
+- I
|
`- ON_
   |
   +- ORDER_
   |  |
   |  `- ADJ_
   |     |
   |     +- strawberry
   |     |
   |     `- cake
   |
   `- Sundays
MAY_
|
+- I
|
`- ON_
   |
   +- ORDER_
   |  |
   |  `- ADJ_
   |     |
   |     +- chocolate
   |     |
   |     `- ice cream
   |
   `- Saturdays
MAY_
|
+- I
|
`- ON_
   |
   +- ORDER_
   |  |
   |  `- ADJ_
   |     |
   |     +- chocolate
   |     |
   |     `- ice cream
   |
   `- Sundays
MAY_
|
+- I
|
`- ON_
   |
   +- ORDER_
   |  |
   |  `- ADJ_
   |     |
   |     +- chocolate
   |     |
   |     `- cake
   |
   `- Saturdays
MAY_
|
+- I
|
`- ON_
   |
   +- ORDER_
   |  |
   |  `- ADJ_
   |     |
   |     +- chocolate
   |     |
   |     `- cake
   |
   `- Sundays
*DataTree PPrint>

-}