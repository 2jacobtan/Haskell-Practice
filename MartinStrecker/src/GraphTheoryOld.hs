{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text.Lazy as T (pack)
import Data.Graph.Inductive.Graph hiding ((&))
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
-- import Data.GraphViz.Printing
-- import Data.GraphViz.Commands
import Data.GraphViz.Attributes.Complete
import Data.Function ((&))
import Data.List (intersect, iterate', (\\))
import System.Environment (getArgs)

-- slides p14
smallGrNodes :: [LNode String]
smallGrNodes = [(0, "A"), (1, "B"), (2, "C"), (3, "G")]
smallGrEdges :: [LEdge Int]
smallGrEdges = [(0, 1, 3), (1, 2, 4), (1, 3, 2),
  (2, 3, 5), (3, 2, 5)]
smallGraph :: Gr String Int
smallGraph = mkGraph smallGrNodes smallGrEdges

-- slides p16
smallGrDot :: IO FilePath
smallGrDot = runGraphviz
  (graphToDot quickParams smallGraph) Pdf "graph.pdf"

-- abstracted above
outputGrDot :: (Graph gr, Labellable l, Labellable el) => gr l el -> FilePath -> IO FilePath
outputGrDot graph = runGraphviz
  (graphToDot quickParams graph) Pdf

-- slides p42 to p43
depthfirst :: (t -> [t]) -> (t -> Bool) -> t -> [t]
depthfirst nexts sol x = dfs [x]
  where
    dfs [] = []
    dfs (nd:nds) =
      if sol nd
      then nd : dfs (nexts nd ++ nds)
      else dfs (nexts nd ++ nds)

breadthfirst :: (t -> [t]) -> (t -> Bool) -> t -> [t]
breadthfirst nexts sol x = bfs [x]
  where
    bfs [] = []
    bfs (nd:nds) =
      if sol nd
      then nd : bfs (nds ++ nexts nd)
      else bfs (nds ++ nexts nd)

-- tutorial p2
data EdgeListGraph a = ELG [a] [(a, a)]
  deriving (Eq, Ord, Show , Read)

data NextFunGraph a = NFG [a] (a -> [a])

smallGrELG :: EdgeListGraph String
smallGrELG =
  ELG ["A", "B", "C", "G"] [("A", "B"), ("B", "C"), ("B", "G"), ("C", "G"), ("G", "C")]

smallGrNexts :: String -> [String]
smallGrNexts x = case x of
  "A" -> ["B"]
  "B" -> ["C", "G"]
  "C" -> ["G"]
  "G" -> ["C"]
  _ -> []
smallGrNFG :: NextFunGraph String
smallGrNFG = NFG ["A", "B", "C", "G"] smallGrNexts

-- Exercise 2
edgeListGraphToGr :: forall a. (Eq a) => EdgeListGraph a -> Gr a String
edgeListGraphToGr (ELG nodes edges) = mkGraph grNodes grEdges
  where
    grNodes :: [(Int, a)]
    grNodes = zip [0..] nodes
    nodeStrToInt :: a -> Int
    nodeStrToInt =
      let mapping = zip nodes [0..]
      in fromMaybe (-1) . flip lookup mapping
    grEdges :: [LEdge String]
    grEdges = map (\(x,y) -> (nodeStrToInt x, nodeStrToInt y, ""))
      edges

smallGrDot2 :: IO FilePath
smallGrDot2 = outputGrDot smallGraph2 "graph2.pdf"
  where smallGraph2 = edgeListGraphToGr smallGrELG

-- Exercise 3
nextFunGraphToEdgeListGraph :: NextFunGraph a -> EdgeListGraph a
nextFunGraphToEdgeListGraph (NFG nodes nextsFun) =
  ELG nodes (edgeNextsToEdgeList nextsFun nodes)
  where
    edgeNextsToEdgeList :: (a -> [a]) -> [a] -> [(a, a)]
    edgeNextsToEdgeList f ns = concatMap (\n -> map (n,) (f n)) ns

smallGraph3 :: Gr String String
smallGraph3 = NFG ["A", "B", "C", "G"] smallGrNexts
  & nextFunGraphToEdgeListGraph 
  & edgeListGraphToGr

smallGrDot3 :: IO FilePath
smallGrDot3 = outputGrDot smallGraph3 "graph3.pdf"

-- Exercise 4
type SearchPath a = [a]

liftNextsToPath :: (a -> [a]) -> SearchPath a -> [SearchPath a]
liftNextsToPath nextsFun path =
  [path ++ [next] | next <- nextsFun lastNode]
  where lastNode = last path


-- Exercise 5
liftNextsToPathNoDup :: Eq a => (a -> [a]) -> SearchPath a -> [SearchPath a]
liftNextsToPathNoDup nextsFun path =
  [path ++ [next] | next <- nextsFun lastNode \\ path]
  where lastNode = last path

-- >>> liftNextsToPathNoDup smallGrNexts ["A","B","G"]
-- >>> liftNextsToPathNoDup smallGrNexts ["A","B","G","C"]
-- [["A","B","G","C"]]

-- []

-- copy-paste from tutorial p4
pathsFromToBFS nexts start goal =
  breadthfirst
  (liftNextsToPathNoDup nexts)
  (\pth -> last pth == goal)
  [start]

pathsFromToDFS nexts start goal =
  depthfirst
  (liftNextsToPathNoDup nexts)
  (\pth -> last pth == goal)
  [start]

-- >>> pathsFromToBFS smallGrNexts "A" "G"
-- >>> pathsFromToDFS smallGrNexts "A" "G"
-- >>> pathsFromToBFS smallGrNexts "G" "A"
-- >>> pathsFromToDFS smallGrNexts "G" "A"
-- [["A","B","G"],["A","B","C","G"]]

-- [["A","B","C","G"],["A","B","G"]]

-- []

-- []

-- Section 5: River Crossing
-- Exercise 6
data Side = Ls | Rs
  deriving (Eq, Ord, Show , Read)

data CfgwState = S {c :: Side , f :: Side , g :: Side , w :: Side}
  deriving (Show, Eq)

changeSide :: Side -> Side
changeSide = \case
  Ls -> Rs
  Rs -> Ls

crossCabbage, crossFerryman, crossGoat, crossWolf :: CfgwState -> CfgwState
crossCabbage s@(c -> c0) = s {c = changeSide c0}
crossFerryman s@(f -> c0) = s {f = changeSide c0}
crossGoat s@(g -> c0) = s {g = changeSide c0}
crossWolf s@(w -> c0) = s {w = changeSide c0}

-- Exercise 7
validState :: CfgwState -> Bool
validState S{..}
  | c == g && g /= f = False
  | w == g && g /= f = False
  | otherwise = True

-- Exercise 8
crossBoat :: CfgwState -> [CfgwState]
crossBoat s0@S{..} = filter validState $
  [s1]
  ++ [s1{c = f1} | c == f]
  ++ [s1{g = f1} | g == f]
  ++ [s1{w = f1} | w == f]
  where
    s1 = s0{f = f1}
    f1 = changeSide f

-- >>> crossBoat $ S Ls Ls Ls Rs
-- [S {c = Rs, f = Rs, g = Ls, w = Rs},S {c = Ls, f = Rs, g = Rs, w = Rs}]

-- Exercise 9
riverDFS :: [SearchPath CfgwState]
riverDFS = pathsFromToDFS crossBoat (S Ls Ls Ls Ls) (S Rs Rs Rs Rs)
riverBFS :: [SearchPath CfgwState]
riverBFS = pathsFromToBFS crossBoat (S Ls Ls Ls Ls) (S Rs Rs Rs Rs)

-- >>> riverDFS == riverBFS
-- True

-- Exercise 10

-- toTupleS = (\(S w x y z) -> (w, x, y, z))
-- unTupleS = (\(w, x, y, z) -> S w x y z)

cfgwNodes :: [CfgwState]
cfgwNodes = S <$> [Ls, Rs] <*> [Ls, Rs] <*> [Ls, Rs] <*> [Ls, Rs]
  & filter validState

cfgwGraph = edgeListGraphToGr (nextFunGraphToEdgeListGraph (NFG cfgwNodes crossBoat))

cfgDot :: IO FilePath
cfgDot = outputGrDot cfgwGraph "riverGraph.pdf"

instance Labellable CfgwState where
  toLabelValue s = StrLabel (T.pack (show s))

-- Section 6: n-Queens

-- | get left-ward coordinates of diagonals that coincide with a position
-- | no need right-ward coords because we place new queens from left to right
diagSquares :: (Ord b, Num b) => b -> (b, b) -> [(b, b)]
diagSquares n (i,j) = nw ++ sw
  where
    traverse move =
      iterate' move (i,j)
      & takeWhile (\(i,j) ->0 < i && i <= n && 0 < j && j <= n)
    -- ne = tail $ traverse (\(i,j) -> (i-1,j+1))
    nw = tail $ traverse (\(i,j) -> (i-1,j-1))
    -- se = tail $ traverse (\(i,j) -> (i+1,j+1))
    sw = tail $ traverse (\(i,j) -> (i+1,j-1))
-- >>> diagSquares 4 (2,2)
-- [(1,1),(3,1)]

makeDiagMap :: (Num b, Enum b, Ord b) => b -> Map (b, b) [(b, b)]
makeDiagMap n = M.fromAscList [((i,j), diagSquares n (i,j)) | i <- [1..n], j <- [1..n]]

nextQueens :: Int -> Map (Int,Int) [(Int,Int)] -> [Int] -> [[Int]]
nextQueens n _diagMap xs | length xs == n = []
nextQueens n diagMap xs = map (\x -> xs ++ [x]) $ filter prune options
  where
    len_xs = length xs
    nextColumn = len_xs + 1
    options = [1..n] \\ xs
    spotsTaken = zip xs [1 ..]
    prune x = null $ (diagMap M.! (x,nextColumn)) `intersect` spotsTaken

solveQueens :: Int -> [[Int]]
solveQueens n = depthfirst (nextQueens n diagMap) ((==n). length) []
  where
    diagMap = makeDiagMap n

main :: IO ()
main = do
  xs <- getArgs
  print $ scanl (\xs _ -> xs + 1) 0 $ solveQueens (read $ head xs)

-- data on n-Queens
-- http://www.durangobill.com/N_Queens.html