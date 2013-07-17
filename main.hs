module Main where

import System.Environment
import System.IO

import Anderson
import Anderson.Random
import Graph
import Graph.Gasket
import Graph.Ring

main = do 
  [fbase, ms, ns] <- getArgs
  let (m, n) = (read ms, read ns)
  let g = gasketAdj $ gasketList m
  -- let g = ring m
  let ss = take n $ drsoIterateGraham g (norandom g) (delta g 0)
  mapM_ (\(i,s) -> do 
    putStrLn $ "writing " ++ show i ++ "th state"
    writeFile (fbase ++ "-" ++ show i ++ ".gv") 
      (graphvizColorShow greenred (intensity s) g)) (zip [0..] ss)
      -- (graphvizShow (labelBy s) g)) (zip [1..] ss)
  putStrLn "done"
