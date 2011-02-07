{-|

Miscellaneous little utilites that don't seem to fit anywhere else. -}

module BRC.Solver.Util (byFirst,
                        mapByFirst,
                        bySecond,
                        mapBySecond) where

import Data.List (foldl')
import qualified Data.Map as DM
import Data.Map (Map)

-- | Given a list of pairs, returns a list pairing unique first values
-- with lists of corresponding second values.
byFirst :: (Ord f) => [(f, s)] -> [(f, [s])]
byFirst = DM.toList . mapByFirst

-- | Given a list of pairs, returns a 'Map' from unique first
-- values to lists of corresponding second values.
mapByFirst :: (Ord f) => [(f,s)] -> Map f [s]
mapByFirst = foldl' accumByFirst DM.empty
  where accumByFirst m (first,second) =
          DM.alter (Just . accum second) first m

-- | Given a list of pairs, returns a list pairing unique second values
-- with lists of corresponding first values.
bySecond :: (Ord s) => [(f, s)] -> [(s, [f])]
bySecond = DM.toList . mapBySecond

-- | Given a list of pairs, returns a 'Map' pairing unique second values
-- with lists of corresponding first values.
mapBySecond :: (Ord s) => [(f,s)] -> Map s [f]
mapBySecond = foldl' accumBySecond DM.empty
  where accumBySecond m (first,second) =
          DM.alter (Just . accum first) second m
          
-- | Either starts a list where none existed before, or prepends to an
-- existing one. Intended for use with 'Data.Map.alter'.
accum :: a -> Maybe [a] -> [a]
accum what Nothing = [what]
accum what (Just sofar) = what:sofar
                
