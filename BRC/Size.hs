{-|

Sizes for set-like values. A size can be either finite or infinite,
and all finite sizes are less than the infinite size.  -}

module BRC.Size (Size,
                 infinite,
                 finite,
                 maybeCount,
                 isZero,
                 isFinite,
                 isInfinite) where

import Prelude hiding (isInfinite)

-- | The size of a set-like value. May be finite or infinite.
newtype Size = Size (Maybe Int) deriving (Eq, Show)

-- | The infinite size.
infinite :: Size
infinite = Size Nothing

-- | A finite size with a given element count.
finite :: Int -> Size
finite = Size . Just

-- | The optional element count for a size.
maybeCount :: Size -> Maybe Int
maybeCount (Size mc) = mc

-- | Whether a size is (finite) zero.
isZero :: Size -> Bool
isZero (Size mc) = maybe False (==0) mc

-- | Whether a size is finite.
isFinite :: Size -> Bool
isFinite (Size mc) = maybe False (const True) mc

-- | Whether a size is infinite.
isInfinite :: Size -> Bool
isInfinite (Size mc) = maybe True (const False) mc

-- Note that 'Maybe Int' has a different ordering than we want, since
-- @Nothing < Just n@, but we're using 'Nothing' as the infinite
-- size. That's the reason for the 'newtype'.
instance Ord Size where
  compare (Size Nothing) (Size Nothing) = EQ
  compare (Size Nothing) (Size _) = GT
  compare (Size _) (Size Nothing) = LT
  compare (Size (Just n)) (Size (Just m)) = compare n m

