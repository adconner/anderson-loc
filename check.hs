module Check where

import Control.Monad
import Data.List
import Data.Vector.Unboxed (Vector,(!))
import qualified Data.Vector.Unboxed as V
import Test.QuickCheck
import System.Random

import Graph
import Anderson.Random
import Anderson.Norm

size = 100
thresh :: Fractional a => a
thresh = 0.001

instance (Random a, V.Unbox a, Num a, Eq a) => Arbitrary (Vector a) where
  arbitrary = liftM V.fromList $ vectorOf size $ choose (-1,1)
  shrink v = concatMap (\ i -> if v ! i == 0 then [] else [v V.// [(i,0)]]) [0..size-1]

instance Arbitrary Adj where
  arbitrary = liftM (adj 0 . zip [0..size-1]) $ vectorOf size $ listOf1 $ choose (0,size-1)
  shrink g = [] -- better?

eqS :: StateV -> StateV -> Bool
eqS v w = l2norm2 (v -^ w) < thresh

eq :: (Ord a, Fractional a) => a -> a -> Bool
eq a b = abs (a - b) < thresh

nullS :: StateV -> Bool
nullS v = l2norm2 v < thresh

notNullS = not . nullS

inSubspace :: StateV -> [StateV] -> Bool
inSubspace = curry (nullS . last . uncurry successivegs)

subspaceSubset :: [StateV] -> [StateV] -> Bool
subspaceSubset ss ss' = all (flip inSubspace ss') ss

prop_null :: StateV -> Bool
prop_null v = nullS (v -^ v)

prop_subSelf :: StateV -> Property
prop_subSelf v = notNullS v ==> nullS (subtractProj v v)

prop_subOther :: StateV -> StateV -> Double -> Property
prop_subOther v w a = notNullS w ==> (subtractProj (v +^ (a *^ w)) w 
                                         `eqS` subtractProj v w)

prop_orth :: StateV -> StateV -> Property
prop_orth v w = notNullS w ==> eq 0 (subtractProj v w `dotp` w)

prop_normalize :: StateV -> Bool
prop_normalize = eq 1 . l2norm2 . normalize

prop_inOwn :: StateV -> Gen Bool
prop_inOwn s = do 
  l <- arbitrary
  r <- arbitrary
  return $ inSubspace s (l ++ [s] ++ r)

-- prop_inGs :: 
prop_gsInOld = do 
  ss <- arbitrary
  let ss' = map (\ (x:xs) -> last (successivegs x xs)) $ init (tails ss)
  return $ subspaceSubset ss' ss

prop_oldInGs = do 
  ss <- arbitrary
  let ss' = map (\ (x:xs) -> last (successivegs x xs)) $ init (tails ss)
  return $ subspaceSubset ss ss' 

prop_drsoLinConstant :: Adj -> RandomV -> Double -> StateV -> Bool
prop_drsoLinConstant g r a v = drso g r (a *^ v) `eqS` (a *^ drso g r v)

prop_drsoLinSum :: Adj -> RandomV -> StateV -> StateV -> Bool
prop_drsoLinSum g r v w = drso g r (v +^ w) `eqS` (drso g r v +^ drso g r w)

prop_drsoNoneSubGs :: Int -> Adj -> RandomV -> StateV -> Bool
prop_drsoNoneSubGs ntosub g r s = subspaceSubset (drsoIterateGraham 0 g r s) 
                                                 (drsoIterateGraham ntosub g r s)

prop_drsoGsSubNone :: Int -> Adj -> RandomV -> StateV -> Bool
prop_drsoGsSubNone ntosub g r s = subspaceSubset (drsoIterateGraham ntosub g r s)
                                                 (drsoIterateGraham 0 g r s)
