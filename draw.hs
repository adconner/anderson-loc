module Main.Draw where

import Control.Monad

import System.Environment
import System.IO
import System.Random

import Anderson.Norm
import Anderson.Random
import Graph
-- import Graph.Gasket
import Graph.Ring

main = do 
  [fbase, ms, ns] <- getArgs
  let (m, n) = (read ms, read ns)

  let g = graph m
  
  r <- liftM (genrandom g 0) getStdGen

  let states = take (n+1) $ drsoIterateGraham 0 g r (delta g 0)
  -- let states = take n $ drsoIterate g r (delta g 0)
  mapM_ (\(i,s) -> do 
    putStrLn $ "writing " ++ show i ++ "th state"
    writeFile (fbase ++ "-" ++ show i ++ ".gv") 
      (graphvizColorShow (labelBy r) greenred (intensity s) g)) (zip [0..] states)
      -- (graphvizShow (labelBy s) g)) (zip [0..] states)
  putStrLn "done"
