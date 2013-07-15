module Main where

import System.Environment
import System.IO

import Anderson
import Gasket

main = do 
  [fbase, ms, ns] <- getArgs
  let (m, n) = (read ms, read ns)
  let g = gasketAdj $ gasketList m
  let ss = drsoIterate g (norandom g) (delta g 5) n
  mapM_ (\(i,s) -> do 
    putStrLn $ "writing " ++ show i ++ "th state"
    writeFile (fbase ++ "-" ++ show i ++ ".gv") 
      (graphvizShow (labelBy s) g)) (zip [1..] ss)
      -- (graphvizShow (const "") g)) (zip [1..] ss)
  putStrLn "done"
