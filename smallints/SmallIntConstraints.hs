module SmallIntConstraints (SmallIntConstraint(..),
                            toBRCConstraints,
                            lteRel) where

import BRC.BinRel
import BRC.Constraint
import qualified BRC.FiniteSet as FS
import BRC.SetOf

import SmallInt

-- | The possible binary relations on 'SmallInt'.
data SmallIntConstraint =
  LTE (Either String SmallInt) (Either String SmallInt)
  -- ^ Less than or equal to.
  deriving (Eq, Ord, Show)
           
-- | Converts a 'SmallIntConstraint' into one or more equivalent BRC
-- constraints.
toBRCConstraints :: SmallIntConstraint -> [Constraint String SmallInt]
toBRCConstraints (LTE l r) = [Related (toBRCRelatee l) (toBRCRelatee r)]

toBRCRelatee :: Either String SmallInt -> Relatee String SmallInt
toBRCRelatee = either Variable Constant

-- | 'BRC.BinRel' representation of the less-than-or-equal relation on
-- 'SmallInt'.
lteRel :: BinRel SmallInt (FS.Set SmallInt)
lteRel = BinRel { contains = (<=),
                  leftOf   = lteLeftOf,
                  rightOf  = lteRightOf }
         
-- | @lteLeftOf x@ is the set of @y@ such that @y <= x@, and so
-- includes @x@.
lteLeftOf x =
  if x == minBound then empty else FS.fromList [minBound .. x]

-- | @lteRightOf x@ is the set of @y@ such that @x <= y@, and so does
-- not include @x@.
lteRightOf x =
  if x == maxBound then empty else FS.fromList [succ x .. maxBound]