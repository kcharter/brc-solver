{-# LANGUAGE FlexibleContexts #-}

{-|

The internal state maintained by the solver. -}

module BRC.Solver.State (SolverState,
                         zeros, ones, twos,
                         initialState,
                         getSetFor,
                         putSetFor,
                         modifySetFor) where

import qualified Data.Map as DM

import BRC.Constraint
import BRC.SetOf
import BRC.Solver.ZeroOneTwo

data SolverState v e s =
  SolverState { zeros :: [ZeroVar v e],
                -- ^ The set of zero-variable contraints.
                ones :: [OneVar v e],
                -- ^ The set of one-variable constraints.
                twos :: [TwoVar v e],
                -- ^ The set of two-variable constraints.
                setsByVar :: DM.Map v s 
                -- ^ The recorded sets of possible assignments, by variable.
              }
  
-- | Computes the initial state from a set of input constraints. This
-- transforms the input constraints into zero-variable, one-variable,
-- and two-variable constraints, and assumes all variable assignments
-- are possible.
initialState :: (Ord v) => [Constraint v e] -> SolverState v e s
initialState constraints =
  let (z,o,t) = toZeroOneTwo constraints
  in SolverState { zeros = z,
                   ones = o,
                   twos = t,
                   setsByVar = DM.empty }

-- | Looks up the current set of possible assignments for a
-- variable. If there are no recorded assignments, we assume that the
-- variable is unconstrained, and the result is 'univ'.
getSetFor :: (Ord v, SetOf e s) => v -> SolverState v e s -> s
getSetFor v ss = maybe univ id (DM.lookup v (setsByVar ss))
              

-- | Computes a new state with a new set of possible assignments for a
-- variable.
putSetFor :: (Ord v) => v -> s -> SolverState v e s -> SolverState v e s
putSetFor v s ss =
  ss { setsByVar = DM.insert v s (setsByVar ss) }

-- | Computes a new state by modifying the set of possible assignments
-- for a variable.
modifySetFor :: (Ord v, SetOf e s) =>
                v -> (s -> s) -> SolverState v e s -> SolverState v e s
modifySetFor v f ss = putSetFor v  (f (getSetFor v ss)) ss

                   