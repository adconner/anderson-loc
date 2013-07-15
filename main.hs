module Main where

import System.Environment
import System.IO

import Anderson.Safe
import Graph
import Graph.Gasket

main = do 
  [fbase, ms, ns] <- getArgs
  let (m, n) = (read ms, read ns)
  let g = gasketAdj $ gasketList m
  let ss = drsoIterate g (norandom g) (delta g 0) n
  mapM_ (\(i,s) -> do 
    putStrLn $ "writing " ++ show i ++ "th state"
    writeFile (fbase ++ "-" ++ show i ++ ".gv") 
      (graphvizColorShow greenred (intensity s) g)) (zip [0..] ss)
      -- (graphvizShow (const "") g)) (zip [1..] ss)
  putStrLn "done"
