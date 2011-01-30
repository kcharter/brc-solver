{-# LANGUAGE ScopedTypeVariables #-}

module SetOfProps where

import Control.Monad (liftM2)
import Test.QuickCheck hiding (elements)

import SetOf

runAll :: forall e . forall s . (Show e, Show s, Eq e, SetOf e s) => Gen e -> Gen s -> IO ()
runAll ge gs =
  mapM_ quickCheck [allE (\x -> elements ((singleton x) :: s) == [x]),
                    allS prop_univIsIntersectionIdentity,
                    allS prop_emptyIsIntersectionZero,
                    allS prop_emptyIsUnionIdentity,
                    allS prop_univIsUnionZero,
                    allS prop_intersectionWithSelfIsSelf,
                    allS prop_unionWithSelfIsSelf,
                    allS2 prop_intersectionCommutative,
                    allS3 prop_intersectionAssociative,
                    allS2 prop_unionCommutative,
                    allS3 prop_unionAssociative,
                    allS3 prop_distributivity,
                    allS prop_sameAsReflexive,
                    allS2 prop_sameAsSymmetric,
                    allS3 prop_sameAsTransitive]
    where allE = forAll ge
          allS = forAll gs
          allS2 = forAll gs2 . uncurry
          allS3 = forAll (gPair gs2 gs) . uncurry . uncurry
          gs2 = gPair gs gs
          gPair = liftM2 (,)

prop_univIsIntersectionIdentity :: SetOf e s => s -> Bool
prop_univIsIntersectionIdentity s = (s `intersection` univ) `sameAs` s

prop_emptyIsIntersectionZero :: SetOf e s => s -> Bool
prop_emptyIsIntersectionZero s = (s `intersection` empty) `sameAs` empty

prop_emptyIsUnionIdentity :: SetOf e s => s -> Bool
prop_emptyIsUnionIdentity s = (s `union` empty) `sameAs` s

prop_univIsUnionZero :: SetOf e s => s -> Bool
prop_univIsUnionZero s = (s `union` univ) `sameAs` univ

prop_intersectionWithSelfIsSelf :: SetOf e s => s -> Bool
prop_intersectionWithSelfIsSelf s = (s `intersection` s) `sameAs` s

prop_unionWithSelfIsSelf :: SetOf e s => s -> Bool
prop_unionWithSelfIsSelf s = (s `union` s) `sameAs` s

prop_intersectionCommutative :: SetOf e s => s -> s -> Bool
prop_intersectionCommutative = prop_commutative intersection

prop_intersectionAssociative :: SetOf e s => s -> s -> s -> Bool
prop_intersectionAssociative = prop_associative intersection

prop_unionCommutative :: SetOf e s => s -> s -> Bool
prop_unionCommutative = prop_commutative union

prop_unionAssociative :: SetOf e s => s -> s -> s -> Bool
prop_unionAssociative = prop_associative union

prop_distributivity :: SetOf e s => s -> s -> s -> Bool
prop_distributivity x y z =
  (x `intersection` (y `union` z)) `sameAs`
  ((x `intersection` y) `union` (x `intersection` z))

prop_sameAsSymmetric :: SetOf e s => s -> s -> Bool
prop_sameAsSymmetric x y = (x `sameAs` y) == (y `sameAs` x)

prop_sameAsReflexive :: SetOf e s => s -> Bool
prop_sameAsReflexive x = x `sameAs` x

prop_sameAsTransitive :: SetOf e s => s -> s -> s -> Bool
prop_sameAsTransitive x y z =
  ((x `sameAs` y) && (y `sameAs` z)) `implies` (x `sameAs` z)
  where x `implies` y = not x || y


prop_commutative :: SetOf e s => (s -> s -> s) -> s -> s -> Bool
prop_commutative op x y = (x `op` y) `sameAs` (y `op` x)

prop_associative :: SetOf e s => (s -> s -> s) -> s -> s -> s -> Bool
prop_associative op x y z = ((x `op` y) `op` z) `sameAs` (x `op` (y `op` z))
