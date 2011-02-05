module Main where

import Control.Monad (liftM)
import Test.QuickCheck

import qualified BRC.FiniteSet as FS
import BRC.Constraint (Constraint)

import qualified SetOfTests as SoT
import qualified ZeroOneTwoTests as ZOTT
import qualified AssignmentTests as AT
import Variable

main :: IO ()
main = do SoT.runAll (arbitrary :: Gen Five) (arbitrary :: Gen (FS.Set Five))
          ZOTT.runAll (arbitrary :: Gen (Constraint Variable Five))
          AT.runAll (arbitrary :: Gen Five)

data Five = One | Two | Three | Four | Five deriving (Eq, Ord, Enum, Bounded, Show)

instance Arbitrary Five where
  arbitrary = elements [One .. Five]
  
instance (Ord e, Arbitrary e) => Arbitrary (FS.Set e) where
  arbitrary = FS.fromList `liftM` arbitrary
                            
                            