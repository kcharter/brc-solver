module Main where

import Control.Monad (liftM)
import Test.QuickCheck

import qualified BRC.FiniteSet as FS
import BRC.Constraint (Constraint)

import qualified SetOfProps as SoP
import qualified ZeroOneTwoProps as ZOTP
import Variable

main :: IO ()
main = do SoP.runAll (arbitrary :: Gen Five) (arbitrary :: Gen (FS.Set Five))
          ZOTP.runAll (arbitrary :: Gen (Constraint Variable Five))

data Five = One | Two | Three | Four | Five deriving (Eq, Ord, Enum, Bounded, Show)

instance Arbitrary Five where
  arbitrary = elements [One .. Five]
  
instance (Ord e, Arbitrary e) => Arbitrary (FS.Set e) where
  arbitrary = FS.fromList `liftM` arbitrary
                            
                            