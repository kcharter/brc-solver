{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module BRC.FiniteSet (Set, fromList) where

import qualified Data.Set as DS

import BRC.Size (finite)
import BRC.SetOf

newtype Set a = Set (DS.Set a) deriving (Eq, Ord, Show)

fromList :: (Ord a) => [a] -> Set a
fromList = Set . DS.fromList

instance (Bounded a, Ord a, Enum a) => SetOf a (Set a) where
  univ = Set (DS.fromList [minBound .. maxBound])
  empty = Set (DS.empty)
  singleton = Set . DS.singleton 
  elements (Set s) = DS.toList s
  intersection (Set s) (Set t) = Set (DS.intersection s t)
  union (Set s) (Set t) = Set (DS.union s t)
  sameAs = (==)
  size (Set s) = finite (DS.size s)
  