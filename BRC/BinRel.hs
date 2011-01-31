{-# LANGUAGE MultiParamTypeClasses #-}

{-|

A representation of binary relations as pairs of left-image and
right-image functions. -}

module BRC.BinRel where


import BRC.SetOf (SetOf)

data (SetOf e s) => BinRel e s =
  BinRel {
    contains :: e -> e -> Bool,
    -- ^ A simple membership test. This must be consistent with the left image
    -- and right image functions: @contains rho x y@ is @True@ if and only if
    --
    -- * @y@ is an element of @rightOf rho x@
    --
    -- * @x@ is an element of @leftOf rho y@
    leftOf :: e -> s,
    -- ^ The left image function. @leftOf rel x@ is the set of all @y@ for which @y rho x@.
    rightOf :: e -> s 
    -- ^ The right image function. @rightOf rel x@ is the set of all @y@ for which @x rho y@.
    }
  