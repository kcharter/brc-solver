module Main where

import Control.Monad (liftM)
import Test.QuickCheck

import qualified FiniteSet as FS

import SetOfProps (runAll)

main :: IO ()
main = runAll (arbitrary :: Gen Five) (arbitrary :: Gen (FS.Set Five))

data Five = One | Two | Three | Four | Five deriving (Eq, Ord, Enum, Bounded, Show)

instance Arbitrary Five where
  arbitrary = elements [One .. Five]
  
instance (Ord e, Arbitrary e) => Arbitrary (FS.Set e) where
  arbitrary = FS.fromList `liftM` arbitrary
                            
                            