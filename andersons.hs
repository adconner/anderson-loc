module Anderson.Safe where

import Data.Vector (Vector,(!))
import qualified Data.Vector as V

import Graph

type Energy = Rational
-- newtype StateV = StateV (Vector Energy)
type StateV = Vector Energy

type RandomV = StateV

delta g n = V.generate (numV g) (\i -> if i == n then 1 else 0)

norandom g = V.replicate (numV g) 0

intensity s = (normalize s !)
  where 
    normalize :: StateV -> Vector Double
    normalize v = V.map ((/ (sqrt . fromRational . l2norm2 $ v)) . fromRational) v

(+^) = V.zipWith (+)
neg = V.map negate
v -^ w = v +^ neg w
(*^) a = V.map (a *)

dotp :: StateV -> StateV -> Energy
dotp = curry $ V.foldl' (+) 0 . uncurry (V.zipWith (*))

l2norm2 :: StateV -> Energy
l2norm2 = V.foldl' (+) 0 . V.map (^2)

subtractProj :: StateV -> StateV -> StateV
subtractProj v w = if n2 == 0 then v else v -^ ((dotp v w / l2norm2 w) *^ w)
  where n2 = l2norm2 w

-- subtractProjOrth :: StateV -> StateV -> StateV
-- subtractProjOrth v w = v -^ ((v `dotp` w) *^ w)

gs :: [StateV] -> StateV -> StateV
gs [] s = s
gs (x:xs) s = gs xs (s `subtractProj` x)

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
        next = gs ss (drso g r (head ss))

drsoDists :: [StateV] -> StateV -> [Double]
drsoDists [] _ = []
drsoDists (x:xs) s = (sqrt . fromRational . l2norm2 $ normal) : drsoDists xs normal
  where normal = subtractProj s x
