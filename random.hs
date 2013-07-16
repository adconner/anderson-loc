module Anderson.Random where

import Data.Vector(Vector, (!))
import qualified Data.Vector as V

import Graph

type RandomV = Vector Double

norandom g = V.replicate (numV g) 0
