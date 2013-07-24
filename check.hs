module Check where

import Control.Monad
import Data.List
import Data.Vector.Unboxed (Vector,(!))
import qualified Data.Vector.Unboxed as V
import Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import System.Random

import Graph
import Anderson.Random
import Anderson.Norm

size = 10
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

eqSR :: StateV -> StateV -> P.Result
eqSR v w = if eqS v w then P.succeeded else P.failed {P.reason = show v ++ " \\= " ++ show w}

eqR :: (Show a, Ord a, Fractional a) => a -> a -> P.Result
eqR a b = if eq a b then P.succeeded else P.failed {P.reason = show a ++ " \\= " ++ show b}

nullSR :: StateV -> P.Result
nullSR v = if l2norm2 v < thresh then P.succeeded else P.failed {P.reason = show v ++ " not null"}

inSubspace :: StateV -> [StateV] -> Bool
inSubspace = curry (nullS . last . uncurry successivegs)

subspaceSubset :: [StateV] -> [StateV] -> Bool
subspaceSubset ss ss' = all (flip inSubspace ss') ss

inSubspaceR :: StateV -> [StateV] -> P.Result
inSubspaceR s ss = if inSubspace s ss then P.succeeded else 
  P.failed {P.reason = show s ++ " not in subspace " ++ show ss}

subspaceSubsetR :: [StateV] -> [StateV] -> Property
subspaceSubsetR ss ss' = foldr1 (.&&.) $ map (property . flip inSubspaceR ss') ss

prop_null :: StateV -> P.Result
prop_null v = nullSR (v -^ v)

prop_small g i = do 
  a <- choose (-thresh/2, thresh/2)
  property $ nullSR (a *^ delta g i)

prop_subSelf :: StateV -> Property
prop_subSelf v = not (nullS v) ==> nullSR (subtractProj v v)

prop_subOther :: StateV -> StateV -> Double -> Property
prop_subOther v w a = not (nullS w) ==> (subtractProj (v +^ (a *^ w)) w 
                                         `eqSR` subtractProj v w)

prop_orth :: StateV -> StateV -> Property
prop_orth v w = not (nullS w) ==> eqR 0 (subtractProj v w `dotp` w)

prop_normalize :: StateV -> P.Result
prop_normalize = eqR 1 . l2norm2 . normalize

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

prop_drsoLinConstant :: Adj -> RandomV -> Double -> StateV -> P.Result
prop_drsoLinConstant g r a v = drso g r (a *^ v) `eqSR` (a *^ drso g r v)

prop_drsoLinSum :: Adj -> RandomV -> StateV -> StateV -> P.Result
prop_drsoLinSum g r v w = drso g r (v +^ w) `eqSR` (drso g r v +^ drso g r w)

prop_drsoNoneSubGs :: Int -> Adj -> RandomV -> StateV -> Bool
prop_drsoNoneSubGs ntosub g r s = subspaceSubset (drsoIterateGraham 0 g r s) 
                                                 (drsoIterateGraham ntosub g r s)

prop_drsoGsSubNone :: Int -> Adj -> RandomV -> StateV -> Bool
prop_drsoGsSubNone ntosub g r s = subspaceSubset (drsoIterateGraham ntosub g r s)
                                                 (drsoIterateGraham 0 g r s)
