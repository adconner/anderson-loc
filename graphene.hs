module Graph.Graphene where

import Graph

{-
 - repeating pattern is as follows

  2|  1
 --+-+--
     |
 --+-+--
  3|  4
-}
graph :: Int -> Adj
graph n = adj (0,0,1) $ concat 
  [[((x,y,1), [(x,y,4), (x,y,2),   (x+1,y,2)]),
    ((x,y,2), [(x,y,1), (x-1,y,1), (x,y-1,3)]),
    ((x,y,3), [(x,y,4), (x-1,y,4), (x,y+1,2)]),
    ((x,y,4), [(x,y,1), (x,y,3),   (x+1,y,3)])] | x <- [-n..n], y <- [-n..n] ]
