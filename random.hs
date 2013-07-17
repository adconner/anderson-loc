module Anderson.Random where

import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import System.Random

import Graph

type RandomV = Vector Double


norandom g = V.replicate (numV g) 0

genrandom gen g c = V.fromList $ take (numV g) $ randomRs (-c/2, c/2) gen 
