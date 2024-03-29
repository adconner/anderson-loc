module Graph.Gasket where

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.List as LO
import qualified Data.Vector.Unboxed as V

import Graph

gasket 1 = [[True, True] ++ repeat False,
            [True, False] ++ repeat False] 
              ++ repeat (repeat False)
gasket n = (take s $ transpose (take s g ++ g)) ++ g
  where g = gasket (n-1)
        s = size (n-1) - 1

size 1 = 2
size n = 2 * (size (n-1)) - 1 

showGasket n = intercalate "\n" . take s . map (take s . map r)
  where r True = '*'
        r False = '.'
        s = size n

type Inode = [Int]
data Gasket = Gasket {
    top, left, right :: Inode,
    nodes :: [(Inode, [Inode])]
  } deriving Show

freshNodes g n = Gasket {
  top = n : top g,
  left = n : left g,
  right = n : right g,
  nodes = map (mapn (n:)) (nodes g)
}
  -- where fresh l = let (v, r) = l in (n : v, map (n:) r)

gasketList 1 = Gasket {
    top = [0],
    left = [1],
    right = [2],
    nodes = sort [([0], [[1],[2]]), ([1], [[0],[2]]), ([2],[[0],[1]])]
  }
gasketList n = Gasket {
  top = top topg,
  left = left leftg,
  right = right rightg,
  nodes = foldr nubCombine [] . sort . map (mapn subs) . 
            concat $ [nodes topg
                    , nodes leftg
                    , nodes rightg]
}
  where nubCombine n [] = [n]
        nubCombine (v1, r1) ((v2, r2):xs) | v1 == v2 = (v1, LO.union r1 r2) : xs
        nubCombine n xs = n : xs
        subs v | v == top leftg = left topg
               | v == top rightg = right topg
               | v == left rightg = right leftg
               | otherwise = v
        [topg, leftg, rightg] = map (freshNodes g) [0,1,2]
        g = gasketList (n-1)

mapn f (v, r) = (f v, map f r)

compile :: Gasket -> [(Int, [Int])]
compile (Gasket _ _ _ o) = map ai o
  where
    ai (v, r) = (index v, map index r)
    index [] = 0
    index (x:xs) = x + 3 * index xs

graphvizShowGasket g = "strict graph G {" ++ intercalate "\n" (map gv ns) ++ "\n}"
  where 
    ns = compile g
    gv (v, r) = concatMap ((++) ("\n  " ++ show v ++ " -- ") . show) r

adjacencylistShow g = intercalate "\n" (map al ns)
  where
    ns = compile g
    al (v, r) = show v ++ ": " ++ intercalate ", " (map show r)

gasketAdj g = adj (top g) (nodes g)

graph :: Int -> Adj
graph = gasketAdj . gasketList
