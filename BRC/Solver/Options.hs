module BRC.Solver.Options where

import BRC.Constraint

-- | Options that govern the behavior of the solver.
data SolverOptions v e s = SolverOptions {
  maxTrialsPerVariable :: Int,
  -- ^ When searching for solutions to two-variable constraints, the
  -- maximum number of trial assignments to attempt for the most
  -- recently bound variable before declaring a dead-end for the
  -- current set of trial bindings. This prevents non-termination if
  -- the constraints have no solution.
  formatVariable :: v -> String,
  -- ^ Formats a variable for error messages.
  formatValue :: e -> String,
  -- ^ Formats a value for error messages.
  formatSet :: s -> String,
  -- ^ Formats a set for error messages.
  formatConstraint :: Constraint v e -> String
  -- ^ Formats a constraint for error messages.
  }

-- | A default set of options that limits the maximum trials per
-- variable to 100, and formats
-- 
-- * all variables as @\<?var\>@
--
-- * all values as @\<?val\>@
--
-- * all sets as @\<?set\>@
--
-- * and all constraints as @\<?constraint\>@ 
--
-- Error messages will obviously be pretty useless.
defaultOptions :: SolverOptions v e s
defaultOptions = SolverOptions {
  maxTrialsPerVariable = 100,
  formatVariable = const "<?var>",
  formatValue = const "<?val>",
  formatSet = const "<?set>",
  formatConstraint = const "<?constraint>"
  }
                 
-- | A default set of options for showable variable, value, and set
-- types. Limits the maximum trials per variable to 100, and uses
-- 'show' for each formatting function.
defaultOptionsShow :: (Show v, Show e, Show s) => SolverOptions v e s 
defaultOptionsShow =  SolverOptions {
  maxTrialsPerVariable = 100,
  formatVariable = show,
  formatValue = show,
  formatSet = show,
  formatConstraint = show
  }