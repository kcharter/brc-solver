module BRC.Constraint (Relatee(..), Constraint(..)) where

-- | The two sides of a constraint.
data Relatee v e =
  Variable v
  -- ^ A variable.
  | Constant e
    -- ^ A constant from the underlying domain.
  deriving (Eq, Ord, Show)
             
-- | Expresses that two relatees must be in the underlying relation.
data Constraint v e =
  Related { left, right :: Relatee v e } deriving (Eq, Ord, Show)

