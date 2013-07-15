module Graph where

import Data.List
import Data.Vector(Vector,(!))
import qualified Data.Vector as V

type Node = Int
data Adj = Adj { offset :: Vector Int,
                 dat :: Vector Node }
  deriving Show

numV :: Adj -> Int
numV = V.length . offset

vertices :: Adj -> Vector Node
vertices = V.enumFromN 0 . numV

degree :: Adj -> Node -> Int
degree g v | v + 1 < numV g = offset g ! (v + 1) - offset g ! v
degree g v = V.length (dat g) - offset g ! v

neighbors :: Adj -> Node -> Vector Node
neighbors g v = V.slice (offset g ! v) (degree g v) (dat g)

labelBy s = take 4 . show . (s V.!)

graphvizShow label g  = "strict graph G {" 
    ++ intercalate "\n" (map gv . V.toList . vertices $ g) ++ "\n}"
  where 
    gv v = concatMap (\w -> "\n  " ++ name v ++ " -- " ++ name w) (V.toList . neighbors g $ v)
    name v = "\"" ++ show v ++ "(" ++ label v ++ ")\""

grayscale i = "\"0.000 0.000 " ++ (take 5 . show . f) i ++ "\""
  where f i = 1 - (i + 1)/2 

hue i = "\"" ++ (take 5 . show . f) i ++ " 1.000 1.000\""
  where f i = 1 - (i + 1)/2 

greenred i | i >= 0 = "\"0.333 " ++ (take 5 . show ) i ++ " 1.000\""
greenred i = "\"0.000 " ++ (take 5 . show . abs ) i ++ " 1.000\""

graphvizColorShow color intensity g  = "strict graph G {\n  node [style=filled]\n" 
    ++ intercalate "\n" (map node . V.toList . vertices $ g) ++ "\n\n"
    ++ intercalate "\n" (map gv . V.toList . vertices $ g) ++ "\n}"
  where 
    node v = "  \"" ++ show v ++ "\" [fillcolor=" ++ color (intensity v) ++ "]"
    gv v = concatMap (\w -> "\n  " ++ name v ++ " -- " ++ name w) (V.toList . neighbors g $ v)
    name v = "\"" ++ show v ++ "\""
