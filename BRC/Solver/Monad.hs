{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

{-|

The internal monad for the solver. This monad is both an error monad
and a state monad, maintaining the internal form of the input
constraints and the sets of possible bindings for each variable. -}

module BRC.Solver.Monad (SolverMonad,
                         runOn,
                         getMaxTrialsPerVariable,
                         formatVariable,
                         formatValue,
                         formatSet,
                         formatConstraint,
                         formatConstraints,
                         getBinRel,
                         getZeros,
                         getOnes,
                         getTwos,
                         getPossibleAssignmentsByVariable,
                         getPossibleAssignmentsFor,
                         modifyPossibleAssignmentsFor) where

import Control.Monad (liftM)
import Control.Monad.Error (MonadError, ErrorT, runErrorT)
import Control.Monad.State (MonadState, State, get, modify, evalState)
import Data.List (intercalate)
import qualified Data.Map as DM

import BRC.BinRel
import BRC.Constraint
import BRC.SetOf
import BRC.Solver.Error
import BRC.Solver.Options (SolverOptions)
import qualified BRC.Solver.Options as Opt
import BRC.Solver.ZeroOneTwo (ZeroVar, OneVar, TwoVar, ToConstraint(..))
import BRC.Solver.State

newtype SolverMonad v e s a =
  SolverMonad { runSolver :: ErrorT SolverError (State (SolverState v e s)) a }
  deriving (Monad, Functor, MonadError SolverError, MonadState (SolverState v e s))
           
-- | Runs a computation in a solver monad, using the initial state
-- computed from a list of input constraints. The result is either a
-- solver error or the intended result of the computation.
runOn :: (Ord v) =>
         SolverOptions v e s ->
         BinRel e s ->
         [Constraint v e] ->
         SolverMonad v e s a -> Either SolverError a
runOn options rel constraints m =
  evalState (runErrorT (runSolver m)) (initialState options rel constraints)
           
getSolverOptions :: SolverMonad v e s (SolverOptions v e s)
getSolverOptions = options `liftM` get

getMaxTrialsPerVariable :: SolverMonad v e s Int
getMaxTrialsPerVariable = fmap Opt.maxTrialsPerVariable getSolverOptions

formatVariable :: v -> SolverMonad v e s String
formatVariable var = fmap (($var) . Opt.formatVariable) getSolverOptions

formatValue :: e -> SolverMonad v e s String
formatValue val = fmap (($val) . Opt.formatValue) getSolverOptions

formatSet :: s -> SolverMonad v e s String
formatSet set = fmap (($set) . Opt.formatSet) getSolverOptions

formatConstraint :: (ToConstraint v e p) => p -> SolverMonad v e s String
formatConstraint c = fmap (($ toConstraint c) . Opt.formatConstraint) getSolverOptions

formatConstraints :: (ToConstraint v e p) => [p] -> SolverMonad v e s String
formatConstraints = fmap (intercalate ", ") . mapM formatConstraint

-- | Gets the binary relation used to interpret the constraints.
getBinRel :: SolverMonad v e s (BinRel e s)
getBinRel = rel `liftM` get

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

-- | Gets a map from variables to sets of possible assignments. This
-- may be missing entries for some variables, in which case the
-- variables are unconstrained.
getPossibleAssignmentsByVariable :: SolverMonad v e s (DM.Map v s)
getPossibleAssignmentsByVariable = fmap setsByVar get

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
