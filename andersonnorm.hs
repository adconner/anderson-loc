module Anderson.Norm where

import Control.Monad
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
-- l2norm2 = V.foldl' (+) 0 . V.map (^2)
l2norm2 = liftM2 dotp id id

l2norm :: StateV -> Energy
l2norm = sqrt . l2norm2

normalize v = V.map (/ l2norm v) v

subtractProj :: StateV -> StateV -> StateV
subtractProj v w = if wn2 < 0.001 
                    then error $ "tried to subtract the projection onto short vector " ++ show w 
                    else v -^ ((dotp v w / wn2) *^ w)
  where wn2 = l2norm w

successivegs :: StateV -> [StateV] -> [StateV]
successivegs = scanl subtractProj

dso :: Adj -> StateV -> StateV
dso g s = s +^ V.imap (\n e -> 
  V.sum (V.map (s !) (neighbors g n)) - fromIntegral (degree g n) * e) s

drso :: Adj -> RandomV -> StateV -> StateV
drso g r s = dso g s +^ V.zipWith (*) r s

drsoIterate :: Adj -> RandomV -> StateV -> [StateV]
drsoIterate g r s = iterate (drso g r) s

drsoIterateGraham :: Int -> Adj -> RandomV -> StateV -> [StateV]
drsoIterateGraham ntosub g r s = unfoldr (\ss -> Just (head ss, next ss : ss)) [s]
  where
    next ss = last $ (successivegs . normalize . drso g r $ head ss) $ take ntosub ss

-- todo investigate numerical issues here
drsoNormals :: Int -> StateV -> Adj -> RandomV -> StateV -> [StateV]
drsoNormals ntosub d1 g r s = successivegs d1 $ drsoIterateGraham ntosub g r s
