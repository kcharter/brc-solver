{-# LANGUAGE MultiParamTypeClasses #-}

{-|

A representation of binary relations as pairs of left-image and
right-image functions. -}

module BRC.BinRel where


import BRC.SetOf (SetOf)

data (SetOf e s) => BinRel e s =
  BinRel {
    leftOf :: e -> s,
    -- ^ The left image function. @leftOf rel x@ is the set of all @y@ for which @y rho x@.
    rightOf :: e -> s 
    -- ^ The right image function. @rightOf rel x@ is the set of all @y@ for which @x rho y@.
    }
  