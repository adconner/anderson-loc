module Anderson.Graham where

import Data.List(unfoldr)
import Data.Vector.Unboxed (Vector,(!))
import qualified Data.Vector.Unboxed as V

import Anderson.Random
import Graph

type Energy = Double
-- newtype StateV = StateV (Vector Energy)
type StateV = Vector Energy

delta g n = V.generate (numV g) (\i -> if i == n then 1 else 0)

(+^) = V.zipWith (+)
neg = V.map negate
v -^ w = v +^ neg w
(*^) a = V.map (a *)

dotp :: StateV -> StateV -> Energy
dotp = curry $ V.foldl' (+) 0 . uncurry (V.zipWith (*))

l2norm2 :: StateV -> Energy
l2norm2 = V.foldl' (+) 0 . V.map (^2)

l2norm :: StateV -> Energy
l2norm = sqrt . l2norm2

normalize v = V.map (/ l2norm v) v

subtractProj :: StateV -> StateV -> StateV
subtractProj v w = if l2norm2 w < 0.001 
                    then error $ "tried to subtract the projection onto short vector " ++ show w 
                    else v -^ (dotp v w *^ w)

successivegs :: StateV -> [StateV] -> [StateV]
successivegs = scanl subtractProj

dso :: Adj -> StateV -> StateV
dso g s = V.imap (\n e -> V.sum (V.map (s !) (neighbors g n))) s

drso :: Adj -> RandomV -> StateV -> StateV
drso g r s = normalize $ dso g s +^ V.zipWith (*) r s

drsoIterateGraham :: Adj -> RandomV -> StateV -> [StateV]
drsoIterateGraham g r s = unfoldr (\ss -> Just (head ss, next ss : ss)) [s]
  where
    next ss = last $ successivegs (drso g r (head ss)) $ take 20 ss

-- todo investigate numerical issues here
drsoNormals :: StateV -> Adj -> RandomV -> StateV -> [StateV]
drsoNormals d1 g r s = successivegs d1 $ drsoIterateGraham g r s

drsoDists :: StateV -> Adj -> RandomV -> StateV -> [Double]
drsoDists = curry . curry . curry $ (map l2norm . (uncurry . uncurry . uncurry $ drsoNormals))
