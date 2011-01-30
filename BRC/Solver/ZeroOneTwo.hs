module BRC.Solver.ZeroOneTwo where

import Data.List (foldl')

import BRC.Constraint
       
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
        