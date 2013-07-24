module Graph.Lattice where

import Graph

defaultdim = 2
  
lattice :: Int -> Int -> [([Int], [[Int]])]
lattice 0 _ = [([], [])] -- (mid, [(vertex, neighbors)])
lattice dim size = [(s:v, (s-1:v):(s+1:v):n) | s <- [-size..size], (v, n) <- adj]
  where adj = lattice (dim-1) size

graph :: Int -> Adj
graph = adj (replicate defaultdim 0) . lattice defaultdim
