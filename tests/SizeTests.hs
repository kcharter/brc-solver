module SizeTests (runAll) where

import Prelude hiding (isInfinite)

import Control.Monad (liftM)
import Test.QuickCheck

import BRC.Size

runAll :: IO ()
runAll = do 
     quickCheck prop_infiniteSize
     quickCheck prop_finiteSize
     quickCheck prop_eq
     quickCheck prop_isInfinite
     quickCheck prop_isFinite

prop_infiniteSize :: Size -> Bool
prop_infiniteSize s = s <= infinite

prop_finiteSize :: Int -> Bool
prop_finiteSize n = maybe False (==n) (maybeCount (finite n))

prop_eq :: Size -> Size -> Bool
prop_eq s t =
  (s == t) `iff` (isInfinite s && isInfinite t ||
                  isFinite s && isFinite t && maybeCount s == maybeCount t)

prop_isInfinite :: Int -> Bool
prop_isInfinite n = isInfinite infinite && not (isInfinite (finite n))

prop_isFinite :: Int -> Bool
prop_isFinite n = not (isFinite infinite) && isFinite (finite n)


instance Arbitrary Size where
  arbitrary = frequency [(10, return infinite),
                         (90, finite `liftM` elements [0..100])]

iff :: Bool -> Bool -> Bool 
iff = (==)
