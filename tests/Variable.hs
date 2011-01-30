module Variable (Variable, variable) where

import Control.Monad (liftM, liftM2)
import Test.QuickCheck

newtype Variable = Variable String deriving (Eq, Ord, Show)

variable :: String -> Variable
variable = Variable

instance Arbitrary Variable where
  arbitrary = variable `liftM` variableNames
    where variableNames = liftM2 (:) firstChar suffix
          firstChar = elements ['a'..'z']
          suffix = frequency [(10, return ""),
                              (90, show `liftM` elements [1..100])]