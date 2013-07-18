module Anderson.Random where

import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import System.Random

import Graph

type RandomV = Vector Double

norandom :: Adj -> RandomV
norandom g = V.replicate (numV g) 0

genrandom :: Adj -> Double -> StdGen -> RandomV
genrandom g c gen = V.fromList $ take (numV g) $ randomRs (-c/2, c/2) gen 
