module Main where

import Control.Monad

import System.Environment
import System.IO
import System.Random

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
  
  r <- liftM (genrandom g 1) getStdGen

  let ss = take n $ drsoIterateGraham g r (delta g 0)
  -- let ss = take n $ drsoIterate g r (delta g 0)
  mapM_ (\(i,s) -> do 
    putStrLn $ "writing " ++ show i ++ "th state"
    writeFile (fbase ++ "-" ++ show i ++ ".gv") 
      (graphvizColorShow (labelBy r) greenred (intensity s) g)) (zip [0..] ss)
      -- (graphvizShow (labelBy s) g)) (zip [0..] ss)
  putStrLn "done"
