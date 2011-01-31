module SmallInt (SmallInt(..), safeFromInt) where

import Control.Monad.Error

data SmallInt =
  Zero |
  One |
  Two |
  Three |
  Four |
  Five |
  Six |
  Seven |
  Eight |
  Nine |
  Ten |
  Eleven |
  Twelve |
  Thirteen |
  Fourteen |
  Fifteen |
  Sixteen |
  Seventeen |
  Eighteen |
  Nineteen |
  Twenty deriving (Eq, Ord, Bounded, Enum)
                
instance Show SmallInt where
  show s = show (fromEnum s - fromEnum Zero)

-- | A safe version of 'toEnum' that will produce an error in an error
-- monad rather than calling 'error' if the argument is out of range.
safeFromInt :: (Error e, MonadError e m) => Int -> m SmallInt
safeFromInt i =
  (if i < fromEnum min
   then failMsg (show i ++ " is less than the absolute minimum of " ++ show min ++ ".")
   else if i > fromEnum max
        then failMsg (show i ++ " is greater than the absolute maximum of " ++ show max ++ ".")
        else return (toEnum i))
  where failMsg = throwError . strMsg
        min = minBound :: SmallInt
        max = maxBound :: SmallInt

            
       