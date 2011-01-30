{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module SetOf where

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
