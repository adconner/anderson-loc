module Anderson where

import Data.Vector.Unboxed (Vector,(!))
import qualified Data.Vector.Unboxed as V

import Anderson.Random
import Graph

type Energy = Double
-- newtype StateV = StateV (Vector Energy)
type StateV = Vector Energy

delta g n = V.generate (numV g) (\i -> if i == n then 1 else 0)

intensity s = (normalize s !)
  where 
    normalize :: StateV -> Vector Double
    normalize v = V.map (/ (sqrt . l2norm2 $ v)) v

(+^) = V.zipWith (+)
neg = V.map negate
v -^ w = v +^ neg w
(*^) a = V.map (a *)

dotp :: StateV -> StateV -> Energy
dotp = curry $ V.foldl' (+) 0 . uncurry (V.zipWith (*))

l2norm2 :: StateV -> Energy
l2norm2 = V.foldl' (+) 0 . V.map (^2)

subtractProj :: StateV -> StateV -> StateV
subtractProj v w = if l2norm2 w < 0.001 
                    then error $ "tried to subtract the projection onto short vector " ++ show w 
                    else v -^ ((dotp v w / l2norm2 w) *^ w)

-- subtractProjOrth :: StateV -> StateV -> StateV
-- subtractProjOrth v w = v -^ ((v `dotp` w) *^ w)

grahamschmidt :: [StateV] -> StateV -> StateV
grahamschmidt [] s = s
grahamschmidt (x:xs) s = grahamschmidt xs (s `subtractProj` x)

-- rewrite to avoid nested data parallelism
dso :: Adj -> StateV -> StateV
dso g s = s +^ V.imap (\n e -> 
  V.sum (V.map (s !) (neighbors g n)) - fromIntegral (degree g n) * e) s

dsoSimp :: Adj -> StateV -> StateV
dsoSimp g s = V.imap (\n e -> V.sum (V.map (s !) (neighbors g n))) s

drso :: Adj -> RandomV -> StateV -> StateV
drso g r s = dso g s +^ V.zipWith (*) r s

drsoIterate :: Adj -> RandomV -> StateV -> [StateV]
drsoIterate g r = iterate (drso g r)

drsoIterateGraham :: Adj -> RandomV -> StateV -> Int -> [StateV]
drsoIterateGraham g r s n = s : drsoIterate' g r [s] n
  where
    drsoIterate' _ _ _ 0 = []
    drsoIterate' g r ss n = next : drsoIterate' g r (next : ss) (n-1)
      where 
        next = grahamschmidt ss (drso g r (head ss))

drsoDists :: [StateV] -> StateV -> [Double]
drsoDists [] _ = []
drsoDists (x:xs) s = sqrt (l2norm2 normal) : drsoDists xs normal
  where normal = subtractProj s x
