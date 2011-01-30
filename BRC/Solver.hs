module BRC.Solver (Relatee(..), Constraint(..), Binding(..), solve) where

import Data.List (foldl')

import BRC.BinRel
import BRC.SetOf

data Relatee v e =
  Variable v
  | Constant e
    deriving (Eq, Ord, Show)
             
data Constraint v e = Related { left, right :: Relatee v e } deriving (Eq, Ord, Show)

data Binding v e = Binding { variable :: v, value :: e } deriving (Eq, Ord, Show)

solve :: (Ord v, SetOf e s) => BinRel e s -> [Constraint v e] -> [[Binding v e]]
solve = error "not implemented"

-- internal state/error monad for the solver (so we can return an error message)
-- split constraints into three simpler constraint types: zero-variable, one-variable, two-variable
-- solve ins three passes:
-- 1. make sure there are no contradictions in zero-variable constraints
-- 2. build up a mapping of variables to sets using the single-variable constraints
-- 3. lazily enumerate Cartesian product, filtered using two-variable constraints
--    - stack to allow back-tracking?

-- | An internal constraint with no variables, just constants.
data NoVars e = NoVars e e;

-- | An internal constraint with exactly one variable and one constant.
data OneVar v e = OnLeft v e | OnRight e v;

-- | An internal constraint with exactly two variables
data TwoVars v = TwoVars v v;

-- | Transforms a list of constraints into lists of zero-variable,
-- one-variable, and two-variable constraints.
splitConstraints :: [Constraint v e] -> ([NoVars e], [OneVar v e], [TwoVars v])
splitConstraints =
  reverse3 . foldl' splitOne ([],[],[])
  where splitOne (zeros, ones, twos) c =
          case left c of
              Variable v ->
                case right c of
                  Variable w ->
                    (zeros, ones, (TwoVars v w):twos)
                  Constant x ->
                    (zeros, (OnLeft v x):ones, twos)
              Constant x ->
                case right c of
                  Variable w ->
                    (zeros, (OnRight x w):ones, twos)
                  Constant y ->
                    ((NoVars x y):zeros, ones, twos)
        reverse3 (xs,ys,zs) = (reverse xs, reverse ys, reverse zs)