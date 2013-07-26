module Graph where

import Data.List
import Data.Vector.Unboxed (Vector,(!))
import qualified Data.Vector.Unboxed as V
import Numeric

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

adj :: Eq a => a -> [(a,[a])] -> Adj
adj start g = Adj (V.fromList shape) (V.fromList dat)
  where
    shape = init $ scanl (+) 0 $ map (length . neighbors) order
    dat = concatMap (sort . map (maybe (error "Graph.adj: vertex not ordered") id
      . flip findIndex order . (==)) . neighbors) order
    order = bfs [start] []
    bfs [] vs | null rem = []
              | otherwise = bfs [head rem] vs
      where rem = vertices \\ vs 
    bfs (n:ns) vs = n : bfs (ns ++ ((neighbors n \\ vs) \\ ns)) (n : vs)
    neighbors n = delete n $ (nub . filter (`elem` vertices) . maybe (error "no vertex") id $ lookup n g)
                      `union` [i | (i,l) <- g, n `elem` l]
    vertices = map fst g

-- labelBy s = (\ d -> if d == 0 then "" else showFFloat (Just 2) d "") . (s V.!)
labelBy s = flip (showFFloat (Just 2)) "" . (s V.!)

empty = const ""

intensity s = (normalize s !)
  where 
    normalize v = V.map (/ (V.maximum . V.map abs) v) v

graphvizShow label g  = "strict graph G {" 
    ++ intercalate "\n" (map gv . V.toList . vertices $ g) ++ "\n}"
  where 
    gv v = concatMap (\w -> "\n  " ++ name v ++ " -- " ++ name w) (V.toList . neighbors g $ v)
    name v = "\"" ++ show v ++ "(" ++ label v ++ ")\""

grayscale i = "\"0.000 0.000 " ++ (show . f) i ++ "\""
  where f i = 1 - (i + 1)/2 

hue i = "\"" ++ (show . f) i ++ " 1.000 1.000\""
  where f i = 1 - (i + 1)/2 

greenred i | i >= 0 = "\"0.333 " ++ show i ++ " 1.000\""
greenred i = "\"0.000 " ++ (show . abs) i ++ " 1.000\""

graphvizColorShow label color intensity g  = "strict graph G {\n  node [style=filled]\n" 
    ++ intercalate "\n" (map node . V.toList . vertices $ g) ++ "\n\n"
    ++ intercalate "\n" (map gv . V.toList . vertices $ g) ++ "\n}"
  where 
    node v = "  " ++ show v ++ " [fillcolor=" ++ color (intensity v) ++ ", label=\"" ++ label v ++ "\"]"
    gv v = concatMap (\w -> "\n  " ++ show v ++ " -- " ++ show w) (V.toList . neighbors g $ v)
