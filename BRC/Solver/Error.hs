{-# LANGUAGE FlexibleContexts #-}

module BRC.Solver.Error where

import Control.Monad.Error (Error(..), MonadError(..))

-- | Solver errors, really just a container for a possibly useful error message.
data SolverError = SolverError String deriving (Eq, Ord)

instance Show (SolverError) where
  show (SolverError msg) = "Solver error: " ++ msg
  
instance Error SolverError where
  strMsg = SolverError
  
-- | Throws an error with a given message in a solver error monad.
solverError :: MonadError SolverError m => String -> m a
solverError = throwError . strMsg