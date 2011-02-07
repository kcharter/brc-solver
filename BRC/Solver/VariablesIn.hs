{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

{-|

Tools for extracting unique variables from various types that contain them. -}

module BRC.Solver.VariablesIn where

import Data.List (nub)
import qualified Data.Set as DS

import BRC.Constraint

-- | A class for types that contain variables, and can say what those
-- variables are.
class VariablesIn v t | t -> v where
  variablesIn :: t -> [v]
  -- ^ The list of unique variables in the input.

instance VariablesIn v (Relatee v e) where
  variablesIn (Variable v) = [v]
  variablesIn (Constant _) = []
  
instance (Eq v) => VariablesIn v (Constraint v e) where
  variablesIn (Related a b) = nub (variablesIn a ++ variablesIn b)
                                                                  
instance (Ord v, VariablesIn v t) => VariablesIn v [t] where
  variablesIn = unique . concatMap variablesIn

instance (Ord v, VariablesIn v a, VariablesIn v b) => VariablesIn v (a,b) where
  variablesIn (a,b) = unique (variablesIn a ++ variablesIn b)
  
unique :: (Ord v) => [v] -> [v]
unique = DS.toList . DS.fromList
