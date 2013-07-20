module Main.Draw where

import Control.Monad

import System.Environment
import System.IO
import System.Random
import System.Console.GetOpt

import Anderson.Norm
import Anderson.Random
import Graph
import Graph.Gasket
-- import Graph.Ring

data Render = Color | Label
data Mode = Iterates | Differences Int
data Options = Options {
  m, n, d0, ntosub :: Int,
  c :: Double,
  mode :: Mode,
  rend :: Render,
  file :: String
}

defaults = Options {
  m = 5,
  n = 30,
  d0 = 0,
  ntosub = 2,
  c = 0,
  mode = Iterates,
  rend = Color,
  file = "pics/graph"
}

render :: Options -> IO ()
render opts = do
  let g = graph $ m opts
  
  r <- if (c opts) == 0 
    then return (norandom g) 
    else liftM (genrandom g (c opts)) getStdGen

  let states = case mode opts of {
    Iterates -> take (n opts + 1) $ drsoIterateGraham (ntosub opts) g r (delta g (d0 opts));
    Differences d1 -> take (n opts + 1) $ drsoNormals (ntosub opts) (delta g d1) g r (delta g (d0 opts))
  }

  let padTo = length $ show (n opts)

  mapM_ (\(i,s) -> do 
    putStrLn $ "writing " ++ show i ++ "th state"
    let is = show i
    writeFile (file opts ++ "-" ++ replicate (padTo - length is) '0' ++ is ++ ".gv") $
      case rend opts of
        Color -> (graphvizColorShow (labelBy r) greenred (intensity s) g)
        Label -> (graphvizShow (labelBy s) g)) 
        (zip [0..] states)
  putStrLn "done"

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['n'] ["iterations"]
      (OptArg (maybe id (\ arg opt -> opt {n = read arg})) "n")
      "number of iterations, graphs generated will be one more than this value"
  , Option ['m'] ["graph-size"]
      (OptArg (maybe id (\ arg opt -> opt {m = read arg})) "size")
      "graph size to use (graph dependant)"
  , Option ['c'] ["random"]
      (OptArg (maybe id (\ arg opt -> opt {c = read arg})) "bound")
      "Discrete schodinger operator randomness bound" 
  , Option ['0'] ["d0"]
      (OptArg (maybe id (\ arg opt -> opt {d0 = read arg})) "i")
      "vector to start iteration with, expressed as a delta index"
  , Option ['1'] ["d1"]
      (OptArg (maybe id (\ arg opt -> opt {mode = Differences (read arg)})) "i")
      "vector to compare iterated subspaces to, expressed as a delta index."
  , Option ['N'] ["ntosub"]
      (OptArg (maybe id (\ arg opt -> opt {ntosub = read arg})) "num")
      "Number of previous state vectors to subtract out by after computing the new state" 
  , Option ['f'] ["fbase"] 
      (OptArg (maybe id (\ arg opt -> opt {file = arg})) "FILE")
      "basename for generated graphs" 
  , Option ['C'] ["color"] (NoArg (\ opt -> opt {rend = Color}))
      "Render graph values as variated colors"
  , Option ['L'] ["label"] (NoArg (\ opt -> opt {rend = Label}))
      "Render graph values as vertex labels"
  ]
    
getOpts :: [String] -> Options
getOpts argv = case getOpt Permute options argv of
  (o,[],[]) -> foldl (flip ($)) defaults o
  (_,_,errs) -> error (concat errs ++ usageInfo "Usage:" options)

main = liftM getOpts getArgs >>= render
