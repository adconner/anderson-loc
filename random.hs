module Anderson.Random where

import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V

import Graph

type RandomV = Vector Double

norandom g = V.replicate (numV g) 0
