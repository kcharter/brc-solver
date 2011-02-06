{-# LANGUAGE ScopedTypeVariables #-}

module AssignmentTests where

import Control.Arrow (second)
import Control.Monad (liftM, liftM2)
import Data.Ord (comparing)
import Data.List (foldl', nub, sortBy, sort)
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Maybe (mapMaybe, catMaybes)
import Test.QuickCheck

import BRC.Solver.Assignments (subjectTo, defaultSearchOptions, maxTrialsPerVariable)

import Variable

runAll :: (Bounded a, Enum a, Ord a, Show a) => Gen a -> IO ()
runAll g = mapM_ quickCheck [
  forAll (assignment g) prop_findsUniqueSolutions,
  forAll (satisfiable g) prop_findsAllSolutions
  ]

prop_findsUniqueSolutions :: (Bounded a, Enum a, Ord a, Show a) =>
                             [(Variable, a)] -> Bool
prop_findsUniqueSolutions assignment =
  let assignment' = DM.toList $  DM.fromList $ assignment
      oneVarConstraints = oneVarLessThans assignment'
      solutions = allSolutions oneVarConstraints
  in length solutions == 1 && assignment' == head solutions

prop_findsAllSolutions :: (Bounded a, Enum a, Ord a, Show a) =>
                          [LTC a] -> Bool
prop_findsAllSolutions constraints =
  sort (bruteForce constraints) == sort (allSolutions constraints)
  
-- | For a value generator, generates a pairing of variables with
-- values. Each variable occurs at most once.
assignment :: Gen a -> Gen [(Variable, a)]
assignment val = (DM.toList . DM.fromList) `liftM` listOf binding
                 where binding = liftM2 (,) arbitrary val

-- | For a value generator, generates an assignment and then derives a
-- set of two-variable less-than constraints from the
-- assignment. There is at least the original solution, so the
-- constraints are satisfiable. This generator takes into account the
-- size of the domain and avoids Cartesian products that have more
-- than 100000 elements.
satisfiable :: forall a .(Bounded a, Enum a, Ord a) => Gen a -> Gen [LTC a]
satisfiable = liftM twoVarLessThans . resize limit . assignment
  where limit = fromIntegral $ floor $ logBase domainSize 100000
        domainSize = fromIntegral $ intervalWidth (minBound :: a) (maxBound :: a)

-- | Computes all solutions of a set of contraints, ignoring
-- constraints that contain no variables. The variable assignments are
-- sorted by variable.
allSolutions :: forall e .(Bounded e, Enum e, Ord e) => [LTC e] -> [[(Variable, e)]]
allSolutions constraints =
  let predicates = toPredicates constraints
      domainSize = intervalWidth domainMin domainMax
      possibles  = zip allVars (repeat universe)
      allVars    = allVariables constraints
      universe   = [domainMin .. domainMax]
      domainMin  = minBound :: e
      domainMax  = maxBound :: e
      opts       =
        defaultSearchOptions {
          maxTrialsPerVariable = domainSize }
  in map (sortBy (comparing fst)) (subjectTo opts predicates possibles)

-- | Computes all the solutions for a set of constraints by trying all
-- possible assignments. Obviously, this can be very expensive, do
-- don't do it on a big domain.
bruteForce :: forall e . (Bounded e, Enum e, Ord e) => [LTC e] -> [[(Variable, e)]]
bruteForce constraints = filter passesAll allAssignments
  where allAssignments =
          map (zip allVars) (product (length allVars) universe)
        -- Note that 'sequence' is here in the list monad, so it
        -- performs a Cartesian product
        product n range = sequence $ replicate n range
        universe = [(minBound :: e) .. (maxBound :: e)]
        allVars = allVariables constraints
        passesAll assignment =
          all passes constraints
            where passes (LTC a b) =
                    case a of
                      Left varA ->
                        case b of
                          Left varB  -> get varA < get varB
                          Right valB -> get varA < valB
                      Right valA ->
                        case b of
                          Left varB  -> valA < get varB
                          Right valB -> valA < valB 
                  get = (DM.!) bindings
                  bindings = DM.fromList assignment

        
-- | Computes the size of the closed interval bounded by two domain
-- values. This is useful for computing the size of a finite domain.
intervalWidth :: (Bounded e, Enum e) => e -> e -> Int
intervalWidth x y = abs (fromEnum y - fromEnum x) + 1

-- | Combines all the predicates represented by list of
-- constraints. When more than one predicate appears for the same pair
-- of variables, they are conjoined into a single predicate. All pairs
-- of variables are unique in the result.
toPredicates :: (Ord e) => [LTC e] -> [((Variable, Variable), e -> e -> Bool)]
toPredicates =
  map (second conjoinAll) . accumPredicates . mapMaybe asPredicate 
  where accumPredicates = byFirst
        conjoinAll = foldl' conjoin (const (const True))
        conjoin pred1 pred2 x y = pred1 x y && pred2 x y
        
-- | Converts a less-than constraint into the form of predicate
-- accepted by 'subjectTo'.
asPredicate :: (Ord e) => LTC e -> Maybe ((Variable, Variable), e -> e -> Bool)
asPredicate (LTC a b) =
  -- I find the case analysis a bit easier to read than calling 'either'
  case a of
    Left varA ->
      case b of
        Left varB  -> Just ((varA, varB), (<))
        Right valB -> Just ((varA, varA), const (< valB))
    Right valA ->
      case b of
        Left varB  -> Just ((varB, varB), const (valA <))
        Right _    -> Nothing

-- | Given an assignment, builds a list of one-variable less-than
-- constraints that are uniquely solved by the assignment. Each
-- variable is pinned between the (at most) two constants on either
-- side of it.
oneVarLessThans :: (Bounded e, Enum e, Ord e) => [(Variable, e)] -> [LTC e]
oneVarLessThans bindings =
  concatMap oneVarLts (varsByValue bindings)
  where oneVarLts (val, vars) =
          maybe [] (\x -> [lt (valArg x) (varArg y) | y <- vars]) (before val) ++
          maybe [] (\x -> [lt (varArg y) (valArg x) | y <- vars]) (after val)
        before x = if x == minBound then Nothing else Just (pred x)
        after x  = if x == maxBound then Nothing else Just (succ x)

-- | Given an assignment, builds a list of less-than constraints
-- between variables that are assigned adjacent values. For example,
-- if @X@ is assigned @1@ and @Y@ is assigned @14@, and no variable is
-- assigned a value between @2@ and @13@, then the constraints include
-- @X < Y@. The resulting set of constraints is satisfied by the
-- original assignment, but need not be the only possible solution.
twoVarLessThans :: (Bounded e, Enum e, Ord e) => [(Variable, e)] -> [LTC e]
twoVarLessThans bindings =
  twoVarLts (groupEqualVars bindings)
    where twoVarLts = concat . fst . foldl' accumTwoVarLts ([],[])
          accumTwoVarLts (sofar, prevVars) vars = (lts:sofar, vars)
            where lts = [lt (varArg x) (varArg y) |
                         x <- prevVars, y <- vars]

-- | For a list of bindings, the lists of variables assigned to each
-- value.
varsByValue :: (Ord a) => [(Variable, a)] -> [(a, [Variable])]
varsByValue = bySecond

-- | Given a list of pairs, returns a list pairing unique first values
-- with lists of corresponding second values.
byFirst :: (Ord f) => [(f, s)] -> [(f, [s])]
byFirst = DM.toList . foldl' accumByFirst DM.empty
  where accumByFirst m (first,second) =
          DM.alter (Just . accum second) first m

-- | Given a list of pairs, returns a list pairing unique second values
-- with lists of corresponding first values.
bySecond :: (Ord s) => [(f, s)] -> [(s, [f])]
bySecond = DM.toList . foldl' accumBySecond DM.empty
  where accumBySecond m (first,second) =
          DM.alter (Just . accum first) second m
                
-- | Either starts a list where none existed before, or prepends to an
-- existing one. Intended for use with 'Data.Map.alter'.
accum :: a -> Maybe [a] -> [a]
accum what Nothing = [what]
accum what (Just sofar) = what:sofar
                
-- | For a list of bindings, the lists of equal variables, in
-- ascending order of the value to which they are bound.
groupEqualVars :: (Ord a) => [(Variable, a)] -> [[Variable]]
groupEqualVars = map snd . varsByValue

-- | Finds and sorts all the unique variables in a list of
-- constraints.
allVariables :: [LTC e] -> [Variable]
allVariables = DS.toList . DS.fromList . concatMap variables

-- | The unique variables appearing in a constraint.
variables :: LTC e -> [Variable]
variables (LTC a b) =
  nub $ catMaybes [argVariables a, argVariables b]
  where argVariables = either Just (const Nothing)
        
lt = LTC

data LTC e = LTC (Arg e) (Arg e) deriving (Eq, Ord, Show)

type Arg e = Either Variable e

varArg = Left
valArg = Right

