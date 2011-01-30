{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module BRC.Solver.ZeroOneTwo where

import Data.List (foldl')

import BRC.Constraint
       
-- | An internal constraint with no variables, just constants.
data ZeroVar v e = ZeroVar e e;

-- | An internal constraint with exactly one variable and one constant.
data OneVar v e = OnLeft v e | OnRight e v;

-- | An internal constraint with exactly two variables
data TwoVar v e = TwoVar v v;

-- | Transforms a list of constraints into lists of zero-variable,
-- one-variable, and two-variable constraints.
toZeroOneTwo :: [Constraint v e] -> ([ZeroVar v e], [OneVar v e], [TwoVar v e])
toZeroOneTwo =
  reverse3 . foldl' splitOne ([],[],[])
  where splitOne (zeros, ones, twos) c =
          case left c of
              Variable v ->
                case right c of
                  Variable w ->
                    (zeros, ones, (TwoVar v w):twos)
                  Constant x ->
                    (zeros, (OnLeft v x):ones, twos)
              Constant x ->
                case right c of
                  Variable w ->
                    (zeros, (OnRight x w):ones, twos)
                  Constant y ->
                    ((ZeroVar x y):zeros, ones, twos)
        reverse3 (xs,ys,zs) = (reverse xs, reverse ys, reverse zs)

fromZeroOneTwo :: ([ZeroVar v e], [OneVar v e], [TwoVar v e]) -> [Constraint v e]
fromZeroOneTwo (zeros,ones,twos) =
  toConstraints zeros ++ toConstraints ones ++ toConstraints twos

toConstraints :: (Functor f, ToConstraint v e p) => f p -> f (Constraint v e)
toConstraints = fmap toConstraint
  
class ToConstraint v e p | p -> e, p -> v where
  toConstraint :: p -> Constraint v e
  
instance ToConstraint v e (ZeroVar v e) where
  toConstraint (ZeroVar x y) = Related (Constant x) (Constant y)
  
instance ToConstraint v e (OneVar v e) where
  toConstraint (OnLeft v x) = Related (Variable v) (Constant x)
  toConstraint (OnRight x v) = Related (Constant x) (Variable v)

instance ToConstraint v e (TwoVar v e) where
  toConstraint (TwoVar v w) = Related (Variable v) (Variable w)
