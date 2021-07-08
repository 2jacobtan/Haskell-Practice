{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Alga where

import Algebra.Graph
    ( removeEdge, removeVertex, Graph(Connect, Overlay, Vertex) )
import Algebra.Graph.Export.Dot ( exportViaShow )
import Data.GraphViz ()
import Algebra.Graph.ToGraph ( ToGraph(ToVertex) )

type User = Int

data RequestM = AddUser User
              | RemoveUser User
              | ConnectU User User
              | DisconnectU User User

handleRequestM :: RequestM -> Graph User -> Graph User
handleRequestM (AddUser a) = Overlay (Vertex a)
handleRequestM (RemoveUser a) = removeVertex a
handleRequestM (ConnectU a b) = h1 (ConnectU a b)
handleRequestM (DisconnectU a b) = removeEdge a b

handleRequestM' :: RequestM -> Graph User -> Graph User
handleRequestM' (AddUser a) = Overlay (Vertex a)
handleRequestM' (RemoveUser a) = removeVertex a
handleRequestM' (ConnectU a b) = h2 (ConnectU a b)
handleRequestM' (DisconnectU a b) = removeEdge a b

h1 :: RequestM -> Graph User -> Graph User
h1 (ConnectU a b) = Overlay (Connect (Vertex a) (Vertex b))
h1 _ = undefined 

h2 :: RequestM -> Graph User -> Graph User
h2 (ConnectU a b) = (=<<) (\x -> if x == a
    then Connect (Vertex x) (Vertex b)
    else Vertex x )
h2 _ = undefined 

g1 :: Graph Int
g1 = Connect 1 2

showG :: (Ord (ToVertex g), Show (ToVertex g), ToGraph g) => g -> String
showG = exportViaShow @String

main :: IO ()
main = putStr $ showG $ handleRequestM (ConnectU 1 3) g1

main2 :: IO ()
main2 = putStr $ showG $ handleRequestM' (ConnectU 1 3) g1

{-
[1 of 1] Compiling Alga             ( /home/jt2/repos/Haskell-Practice/algebraic-graphs-experiment/src/Alga.hs, interpreted )
Ok, one module loaded.
*Alga> main
digraph 
{
  "1"
  "2"
  "3"
  "1" -> "2"
  "1" -> "3"
}
*Alga> main2
digraph 
{
  "1"
  "2"
  "3"
  "1" -> "2"
  "1" -> "3"
  "3" -> "2"
}
-}