{-# LANGUAGE TupleSections #-}

module BRC.Solver (Relatee(..), Constraint(..), solve) where

import Control.Monad (unless, when, foldM)
import Data.List (foldl')
import qualified Data.Map as DM

import BRC.BinRel
import BRC.Constraint
import BRC.SetOf
import BRC.Size
import qualified BRC.Solver.Assignments as A
import BRC.Solver.Error
import BRC.Solver.Influence
import BRC.Solver.Monad
import BRC.Solver.Options (SolverOptions)
import BRC.Solver.VariablesIn
import BRC.Solver.Util (mapByFirst)
import BRC.Solver.ZeroOneTwo

solve :: (Ord v, Ord e, SetOf e s) =>
         SolverOptions v e s -> BinRel e s -> [Constraint v e] -> Either SolverError [[(v,e)]]
solve options rel constraints =
  runOn options rel constraints $ do
    ruleOutZeroVarContradictions rel
    applyOneVarConstraints rel
    enumerateAssignments rel

ruleOutZeroVarContradictions :: (SetOf e s) =>
                                BinRel e s -> SolverMonad v e s ()
ruleOutZeroVarContradictions rel =
  mapM_ checkForContradiction =<< getZeros
  where checkForContradiction c@(ZeroVar x y) =
          unless (contains rel x y) $ do
            cfmt <- formatConstraint c
            solverError ("Constraint " ++ cfmt ++ " is a contradiction.")

applyOneVarConstraints :: (Ord v, SetOf e s) =>
                          BinRel e s -> SolverMonad v e s ()
applyOneVarConstraints rel =
  getOnes >>= foldM refineOneVar DM.empty >> return ()
  where refineOneVar history c =
          do let (v, s) = varAndSet c
             modifyPossibleAssignmentsFor v (intersection s)
             let history' = recordConstraint v history
             checkForEmpty v history'
             return history'
            where varAndSet (OnLeft v x) = (v, leftOf rel x)
                  varAndSet (OnRight x v) = (v, rightOf rel x)
                  checkForEmpty v history =
                    getPossibleAssignmentsFor v >>= \s ->
                    when (s `sameAs` empty) $ do
                      csfmt <- formatConstraints (constraintsSoFar v history)
                      solverError ("Constraints " ++ csfmt ++ " are unsatisfiable.")
                  recordConstraint v history =
                    DM.alter (maybe (Just [c]) (Just . (c:))) v history
                  constraintsSoFar v history =
                    maybe [] reverse (DM.lookup v history)

enumerateAssignments :: (Ord v, Ord e, SetOf e s) => BinRel e s -> SolverMonad v e s [[(v,e)]]
enumerateAssignments rel = do
  vars1 <- fmap variablesIn getOnes
  twos <- getTwos
  maxTrialsPerVar <- getMaxTrialsPerVariable
  byVar <- getPossibleAssignmentsByVariable
  binRel <- getBinRel
  let classes = influenceClasses vars1 twos
      classSolver = solveClass maxTrialsPerVar binRel byVar twos
  return (map concat $ sequence $ map classSolver classes)
            
solveClass :: (Ord v, SetOf e s) => Int -> BinRel e s -> DM.Map v s -> [TwoVar v e] -> [v] -> [[(v,e)]]
solveClass maxTrialsPerVar binRel byVar twos classVars =
  case classVars of
    [] -> error "Shouldn't be able to get an empty influence class."
    [v] -> map ((:[]).(v,)) $ elements $ maybe univ id $ DM.lookup v byVar
    _ -> A.assignments options nextVar effects unbounds
      where options = A.defaultSearchOptions {
              A.maxTrialsPerVariable = maxTrialsPerVar
              }
            unbounds = initialUnbounds binRel byVar twos
            
data Unbounds v e s = Unbounds {
  binRel :: BinRel e s,
  constraintsByVar :: DM.Map v [TwoVar v e],
  setsByVar :: DM.Map v s
  }

initialUnbounds :: (Ord v) => BinRel e s -> (DM.Map v s) -> [TwoVar v e] -> Unbounds v e s
initialUnbounds rel setsByVar twos = Unbounds {
  binRel = rel,
  constraintsByVar = mapByFirst $ concatMap (\c -> map (,c) $ variablesIn c) twos,
  setsByVar = setsByVar
  }

nextVar :: (SetOf e s) => Unbounds v e s -> A.NextVar v e (Unbounds v e s)
nextVar u =
  if noMoreVars u
  then A.Done
  else if anyEmptySets u
       then A.DeadEnd
       else let (v, s, u') = splitVarToBind u
                es = elements s
            in A.NextVar v (head es) (tail es) u'

noMoreVars :: Unbounds v e s -> Bool
noMoreVars = (0==) . DM.size . setsByVar

anyEmptySets :: (SetOf e s) => Unbounds v e s -> Bool
anyEmptySets = any (isZero . size . snd) . DM.toList . setsByVar

splitVarToBind :: Unbounds v e s -> (v, s, Unbounds v e s)
splitVarToBind u =
  (v, s, u {
      setsByVar = remaining
      })
  where ((v, s), remaining) = DM.deleteFindMin $ setsByVar u

effects :: (Ord v, SetOf e s) => v -> e -> Unbounds v e s -> Unbounds v e s
effects v x u =
  u {
    setsByVar = DM.mapWithKey effectOn (setsByVar u)
    }
  where effectOn w s =
          effectOfConstraints (binRel u) v x s $ lookupConstraints v w u
  
lookupConstraints :: (Ord v) => v -> v -> Unbounds v e s -> [TwoVar v e]
lookupConstraints v w u =
  maybe [] (filter (involves w)) $ DM.lookup v (constraintsByVar u)
  where involves w = elem w . variablesIn

effectOfConstraints :: (Eq v, SetOf e s) => BinRel e s -> v -> e -> s -> [TwoVar v e] -> s
effectOfConstraints rel v x s =
  foldl' (effectOfConstraint rel v x) s
  
effectOfConstraint :: (Eq v, SetOf e s) => BinRel e s -> v -> e -> s -> TwoVar v e -> s
effectOfConstraint rel v x s c =
  case c of
    TwoVar w z | w == v -> s `intersection` rightOf rel x
               | z == v -> s `intersection` leftOf rel x
               | otherwise -> s

