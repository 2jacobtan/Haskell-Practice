{-# LANGUAGE OverloadedStrings #-}

module DataTree2 where

import Data.Tree
import Data.Text ( unpack, Text )
import Data.String (IsString)

{-
Next evolution, I have a request to distinguish between U-lists and ∩-lists: you can order chocolate, strawberry, or peanut-butter-and-jelly.

So we need to nest:
Or [ Term chocolate, Term strawberry, And [ Term peanutButter, Term jelly ] ]
-}

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

[1 of 1] Compiling DataTree2        ( /home/jt2/repos/Haskell-Practice/DataTree2.hs, interpreted )
Ok, one module loaded.
*DataTree2 PPrint> main
I may order chocolate on Saturdays
I may order strawberry on Saturdays
I may order peanut butter and jelly on Saturdays
MAY_
|
+- I
|
`- ON_
   |
   +- ORDER_
   |  |
   |  `- chocolate
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
   |  `- strawberry
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
   |  `- peanut butter and jelly
   |
   `- Saturdays

*DataTree2 PPrint>

-}