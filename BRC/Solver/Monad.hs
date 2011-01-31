{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

{-|

The internal monad for the solver. This monad is both an error monad
and a state monad, maintaining the internal form of the input
constraints and the sets of possible bindings for each variable. -}

module BRC.Solver.Monad (SolverMonad,
                         runOn,
                         getZeros,
                         getOnes,
                         getTwos,
                         getPossibleAssignmentsFor,
                         modifyPossibleAssignmentsFor) where

import Control.Monad (liftM)
import Control.Monad.Error (MonadError, ErrorT, runErrorT)
import Control.Monad.State (MonadState, State, get, modify, evalState)

import BRC.Constraint
import BRC.SetOf
import BRC.Solver.Error
import BRC.Solver.ZeroOneTwo (ZeroVar, OneVar, TwoVar)
import BRC.Solver.State

newtype SolverMonad v e s a =
  SolverMonad { runSolver :: ErrorT SolverError (State (SolverState v e s)) a }
  deriving (Monad, Functor, MonadError SolverError, MonadState (SolverState v e s))
           
-- | Runs a computation in a solver monad, using the initial state
-- computed from a list of input constraints. The result is either a
-- solver error or the intended result of the computation.
runOn :: (Ord v) => [Constraint v e] -> SolverMonad v e s a -> Either SolverError a
runOn constraints m = evalState (runErrorT (runSolver m)) (initialState constraints)
           
-- | Gets the list of zero-variable constraints derived from the input
-- constraints.
getZeros :: SolverMonad v e s [ZeroVar v e]
getZeros = zeros `liftM` get

-- | Gets the list of one-variable constraints derived from the input
-- constraints.
getOnes :: SolverMonad v e s [OneVar v e]
getOnes = ones `liftM` get

-- | Gets the list of two-variable constraints derived from the input
-- constraints.
getTwos :: SolverMonad v e s [TwoVar v e]
getTwos = twos `liftM` get

-- | Gets the current set of possible assignments for a variable. If
-- the variable is unconstrained, the set will be 'univ'.
getPossibleAssignmentsFor :: (Ord v, SetOf e s) =>
                             v -> SolverMonad v e s s
getPossibleAssignmentsFor v = getSetFor v `liftM` get

-- | Refines the current set of possible assignments for a variable by
-- applying a function.
modifyPossibleAssignmentsFor :: (Ord v, SetOf e s) =>
                                v -> (s -> s) -> SolverMonad v e s ()
modifyPossibleAssignmentsFor v f = modify (modifySetFor v f)
