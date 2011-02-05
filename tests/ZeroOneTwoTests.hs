module ZeroOneTwoTests where

import Control.Monad (liftM, liftM2)
import Data.List (sort)
import Test.QuickCheck

import BRC.Constraint
import BRC.Solver.ZeroOneTwo

runAll :: (Ord v, Ord e, Show v, Show e) => Gen (Constraint v e) -> IO ()
runAll g = quickCheck (forAll (listOf g) prop_toAndFromZeroOneTwo)

prop_toAndFromZeroOneTwo :: (Ord v, Ord e) => [Constraint v e] -> Bool
prop_toAndFromZeroOneTwo cs =
  sort cs == (sort $ fromZeroOneTwo $ toZeroOneTwo cs)
  
instance (Arbitrary v, Arbitrary e) => Arbitrary (Relatee v e) where
  arbitrary = oneof [Variable `liftM` arbitrary,
                     Constant `liftM` arbitrary]
              
instance (Arbitrary v, Arbitrary e) => Arbitrary (Constraint v e) where
  arbitrary = liftM2 Related arbitrary arbitrary
  
