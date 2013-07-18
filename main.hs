module Main.Anderson where

import Control.Monad

import System.Environment
import System.IO
import System.Random
import Numeric

-- import Anderson
-- import Anderson.Norm
import Anderson.Graham
import Anderson.Random
import Graph
import Graph.Gasket
-- import Graph.Ring

main = do 
  [ms, ns, cs, d0s, d1s] <- getArgs
  let (m, n, c, d0, d1) = (read ms, read ns, read cs, read d0s, read d1s)

  let g = graph m
  
  r <- liftM (genrandom g c) getStdGen

  let dists = take (n+1) $ drsoDists (delta g d1) g r (delta g d0)
  
  mapM_ (\(n, d) -> putStrLn $ show n ++ " " ++ showFFloat (Just 16) d "") $ zip [0..] dists
