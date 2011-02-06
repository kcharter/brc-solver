{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module BRC.SetOf where

import Prelude hiding (isInfinite)

import BRC.Size

class SetOf e s | s -> e where
  univ :: s
  -- ^ The universe.
  empty :: s
  -- ^ The empty set.
  singleton :: e -> s
  -- ^ Makes a singleton set containing a given element.
  elements :: s -> [e]
  -- ^ The (lazy) list of elements of a set. Need not be finite.
  intersection :: s -> s -> s
  -- ^ Set intersection.
  union :: s -> s -> s
  -- ^ Set union.
  sameAs :: s -> s -> Bool
  -- ^ An equality test. For instances for which 'Eq s', this will
  -- probably be '(==)'. We don't require it, however.
  size :: s -> Size
  -- ^ The size of the set. Note the following constraints:
  --
  -- * @isZero (size empty)@
  --
  -- * @size empty <= size univ@
  --
  -- * @size x <= size (x `intersection` y)@
  --
  -- * @isFinite (size x)@ implies that @elements x@ is a finite list
  -- whose length is the element count of @size x@.