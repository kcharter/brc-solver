module AssignmentTests where

import Test.QuickCheck

runAll :: (Bounded a, Ord a, Show a) => Gen a -> IO ()
runAll g = return ()

