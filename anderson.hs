module Anderson where

import Data.Vector (Vector,(!))
import qualified Data.Vector as V

import Graph

type Energy = Double
-- newtype StateV = StateV (Vector Energy)
type StateV = Vector Energy

type RandomV = StateV

delta g n = V.generate (numV g) (\i -> if i == n then 1 else 0)

norandom g = V.replicate (numV g) 0

(+^) = V.zipWith (+)
neg = V.map negate
v -^ w = v +^ neg w
(*^) a = V.map (a *)

dotp :: StateV -> StateV -> Energy
dotp = curry $ V.foldl' (+) 0 . uncurry (V.zipWith (*))

l2norm2 :: StateV -> Energy
l2norm2 = V.foldl' (+) 0 . V.map (^2)

-- subtractProj :: StateV -> StateV -> StateV
-- subtractProj v w = v -^ ((dotp v w / l2norm2 w) *^ w)

subtractProjOrth :: StateV -> StateV -> StateV
subtractProjOrth v w = v -^ ((v `dotp` w) *^ w)

gsOrth :: [StateV] -> StateV -> StateV
gsOrth xs s = (1 / sqrt (l2norm2 res)) *^ res
  where 
    res = f xs s
    f [] s = s
    f (x:xs) s = f xs (s `subtractProjOrth` x)

-- rewrite to avoid nested data parallelism
dso :: Adj -> StateV -> StateV
dso g s = V.imap (\n e -> 
  V.sum (V.map (s !) (neighbors g n)) - fromIntegral (degree g n) * e) s

dsoSimp :: Adj -> StateV -> StateV
dsoSimp g s = V.imap (\n e -> V.sum (V.map (s !) (neighbors g n))) s

drso :: Adj -> RandomV -> StateV -> StateV
drso g r s = dso g s +^ V.zipWith (*) r s

drsoIterate :: Adj -> RandomV -> StateV -> Int -> [StateV]
drsoIterate g r s n = s : drsoIterate' g r [s] n
  where
    drsoIterate' _ _ _ 0 = []
    drsoIterate' g r ss n = next : drsoIterate' g r (next : ss) (n-1)
      where 
        next = gsOrth ss (drso g r (head ss))

drsoDists :: [StateV] -> StateV -> [Double]
drsoDists [] _ = []
drsoDists (x:xs) s = sqrt (l2norm2 normal) : drsoDists xs normal
  where normal = subtractProjOrth s x
