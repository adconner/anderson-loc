module Main where

import Control.Monad

import System.Environment
import System.IO
import System.Random
import System.Console.GetOpt
import Numeric

import Anderson.Norm
import Anderson.Random
import Graph
import Graph.Gasket
-- import Graph.Ring

data Render = None | Color | Label deriving Eq
data Mode = Iterates | Differences Int deriving Eq
data Options = Options {
  m, n, d0, ntosub :: Int,
  c :: Double,
  mode :: Mode,
  rend :: Render,
  file :: String
} deriving Eq

defaults = Options {
  m = 5,
  n = 30,
  d0 = 0,
  ntosub = 2,
  c = 0,
  mode = Iterates,
  rend = None,
  file = "pics/graph"
}

render :: Options -> IO ()
render opts = do
  let g = graph $ m opts
  
  r <- if (c opts) == 0 
    then return (norandom g) 
    else liftM (genrandom g (c opts)) getStdGen

  let states = case mode opts of
        Iterates -> take (n opts + 1) $ drsoIterateGraham (ntosub opts) g r (delta g (d0 opts))
        Differences d1 -> take (n opts + 1) $ drsoNormals (ntosub opts) (delta g d1) g r (delta g (d0 opts))

  let padTo = length $ show (n opts)

  mapM_ (\(i,s) -> do 
    putStrLn $ show i ++ " " ++ showFFloat (Just 16) (l2norm s) ""
    let is = show i
    let todisk = writeFile (file opts ++ "-" ++ replicate (padTo - length is) '0' ++ is ++ ".gv")
    case rend opts of
      None -> return ()
      Color -> todisk $ (graphvizColorShow (labelBy r) greenred (intensity s) g)
      Label -> todisk $ (graphvizShow (labelBy s) g))
        (zip [0..] states)
  putStrLn "done"

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['n'] ["iterations"]
      (OptArg (maybe id (\ arg opt -> opt {n = read arg})) "n")
      ("number of iterations, graphs generated will be one more than this value (default: " ++ show (n defaults) ++ ")")
  , Option ['m'] ["graph-size"]
      (OptArg (maybe id (\ arg opt -> opt {m = read arg})) "size")
      ("graph size to use (graph dependant) (default: " ++ show (m defaults) ++ ")")
  , Option ['c'] ["random"]
      (OptArg (maybe id (\ arg opt -> opt {c = read arg})) "bound")
      ("Discrete schodinger operator randomness bound (default: " ++ show (c defaults) ++ ")")
  , Option ['0'] ["d0"]
      (OptArg (maybe id (\ arg opt -> opt {d0 = read arg})) "i")
      ("vector to start iteration with, expressed as a delta index (default: " ++ show (d0 defaults) ++ ")")
  , Option ['1'] ["d1"]
      (OptArg (maybe id (\ arg opt -> opt {mode = Differences (read arg)})) "i")
      ("vector to compare iterated subspaces to, expressed as a delta index. (default: none)")
  , Option ['N'] ["ntosub"]
      (OptArg (maybe id (\ arg opt -> opt {ntosub = read arg})) "num")
      ("Number of previous states to Graham Schmidt out by after computing new state (default: " 
        ++ show (ntosub defaults) ++ ")")
  , Option ['f'] ["fbase"] 
      (OptArg (maybe id (\ arg opt -> opt {file = arg})) "FILE")
      ("basename for generated graphs (default: " ++ file defaults ++ ")")
  , Option ['C'] ["color"] (NoArg (\ opt -> opt {rend = Color}))
      ("Render graph values as variated colors" ++ if rend defaults == Color then " (default)" else "")
  , Option ['L'] ["label"] (NoArg (\ opt -> opt {rend = Label}))
      ("Render graph values as vertex labels" ++ if rend defaults == Label then " (default)" else "")
  , Option [] ["no-render"] (NoArg (\ opt -> opt {rend = None}))
      ("Do not render graphs" ++ if rend defaults == None then " (default)" else "")
  ]
    
getOpts :: [String] -> Options
getOpts argv = case getOpt Permute options argv of
  (o,[],[]) -> foldl (flip ($)) defaults o
  (_,_,errs) -> error (concat errs ++ usageInfo "Usage:" options)

main = liftM getOpts getArgs >>= render
