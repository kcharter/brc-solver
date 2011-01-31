module BRC.Solver (Relatee(..), Constraint(..), Binding(..), solve) where

import BRC.BinRel
import BRC.Constraint
import BRC.SetOf
import BRC.Solver.Error (SolverError)

data Binding v e = Binding { variable :: v, value :: e } deriving (Eq, Ord, Show)

solve :: (Ord v, SetOf e s) => BinRel e s -> [Constraint v e] -> Either SolverError [[Binding v e]]
solve = error "not implemented"

-- internal state/error monad for the solver (so we can return an error message)
-- split constraints into three simpler constraint types: zero-variable, one-variable, two-variable
-- solve ins three passes:
-- 1. make sure there are no contradictions in zero-variable constraints
-- 2. build up a mapping of variables to sets using the single-variable constraints
-- 3. lazily enumerate Cartesian product, filtered using two-variable constraints
--    - stack to allow back-tracking?

