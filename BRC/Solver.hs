module BRC.Solver (Relatee(..), Constraint(..), Binding(..), solve) where

import Control.Monad (unless, when, foldM)
import Data.List (intercalate)
import qualified Data.Map as DM

import BRC.BinRel
import BRC.Constraint
import BRC.SetOf
import BRC.Solver.Error
import BRC.Solver.Monad
import BRC.Solver.ZeroOneTwo

data Binding v e = Binding { variable :: v, value :: e } deriving (Eq, Ord, Show)

solve :: (Ord v, SetOf e s, Show v, Show e) =>
         BinRel e s -> [Constraint v e] -> Either SolverError [[Binding v e]]
solve rel constraints =
  runOn constraints $ do
    ruleOutZeroVarContradictions rel
    applyOneVarConstraints rel
    enumerateAssignments rel

ruleOutZeroVarContradictions :: (Show v, Show e, SetOf e s) =>
                                BinRel e s -> SolverMonad v e s ()
ruleOutZeroVarContradictions rel =
  mapM_ checkForContradiction =<< getZeros
  where checkForContradiction c@(ZeroVar x y) =
          unless (contains rel x y) $ solverError (
            "Constraint " ++ show (toConstraint c) ++
            " is a contradiction.")

applyOneVarConstraints :: (Show v, Show e, Ord v, SetOf e s) =>
                          BinRel e s -> SolverMonad v e s ()
applyOneVarConstraints rel =
  getOnes >>= foldM refineOneVar DM.empty >> return ()
  where refineOneVar history c =
          do let (v, s) = varAndSet c
             modifyPossibleAssignmentsFor v (intersection s)
             checkForEmpty v
             return $ recordConstraint v
            where varAndSet (OnLeft v x) = (v, leftOf rel x)
                  varAndSet (OnRight x v) = (v, rightOf rel x)
                  checkForEmpty v =
                    getPossibleAssignmentsFor v >>= \s ->
                    when (s `sameAs` empty) (
                      solverError ("Constraints " ++
                                   showConstraints (c:constraintsSoFar v) ++
                                   " are unsatisfiable."))
                  recordConstraint v =
                    DM.alter (maybe (Just [c]) (Just . (c:))) v history
                  constraintsSoFar v =
                    maybe [] id (DM.lookup v history)
                  showConstraints = intercalate ", " . map (show . toConstraint)

enumerateAssignments :: SetOf e s => BinRel e s -> SolverMonad v e s [[Binding v e]]
enumerateAssignments rel = return [] -- TODO: really implement

-- internal state/error monad for the solver (so we can return an error message)
-- split constraints into three simpler constraint types: zero-variable, one-variable, two-variable
-- solve ins three passes:
-- 1. make sure there are no contradictions in zero-variable constraints
-- 2. build up a mapping of variables to sets using the single-variable constraints
-- 3. lazily enumerate Cartesian product, filtered using two-variable constraints
--    - stack to allow back-tracking?

