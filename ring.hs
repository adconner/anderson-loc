module Graph.Ring where

import Graph

ring :: Int -> Adj
ring n = adj 0 $ map (\m -> (m,neighbors m)) [0..n-1]
  where
    neighbors m = map (`mod` n) [m-1, m+1]
