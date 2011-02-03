{-|

Tools for enumerating assignments while observing pair-wise
constraints. The search does all the necessary book-keeping to
backrack when it reaches a dead end. -}

module BRC.Solver.Assignments (assignments,
                               NextVar(..),
                               cartesian,
                               subjectTo) where

import qualified Data.Map as DM

-- | Computes the list of all satisfying assignments to a set of
-- variables, when the value of a variable can influence the possible
-- values of other variables.
--
-- This function is generic for any representation of the unbound
-- variables. Its only requirements are a pair of functions that
--
-- * splits off an unbound variable and its set of possible values, and
--
-- * further restricts the remaining unbound variables when assigning
-- a value to a bound variable
--
-- The approach is a back-tracking search. We make tentative
-- assignments, compute their effects on the remaining unbound
-- variables, and maintain the state necessary to back-track out of
-- dead-ends in the search.
assignments :: (u -> NextVar v e u) -> (v -> e -> u -> u) -> u -> [[(v,e)]]
assignments nextVar effectOfBinding unboundVars =
  doAssignments $ initial nextVar effectOfBinding unboundVars
  where doAssignments p =
          maybe [] (\(a, mp) -> a:maybe [] doAssignments mp) $ nextAssignment p
          
-- | Computes the Cartesian product from a list of possible input
-- bindings, using 'assignments'. This is mostly meant for testing
-- purposes.
cartesian :: [(v, [e])] -> [[(v, e)]]
cartesian = assignments nextVar noEffect
  where nextVar [] = Done
        nextVar p@((v, es):rest) =
          if any null $ map snd p
          then DeadEnd
          else NextVar v (head es) (tail es) rest
        noEffect _ _ u = u

-- | Computes all combinations of variable assignments that pass a list
-- of binary predicates.
subjectTo :: Ord v => [((v,v), e -> e -> Bool)] -> [(v, [e])] -> [[(v,e)]]
subjectTo conditions = assignments nextVar filterEffects
  where nextVar [] = Done
        nextVar p@((v, es):rest) =
          if any null $ map snd p
          then DeadEnd
          else NextVar v (head es) (tail es) rest
        filterEffects v e =
          let doFilter (w, es) =
                let f1 = maybe pass ($e) $ pred (v,w)
                    f2 = maybe pass (($e) . flip) $ pred (v,w)
                    f = if v == w then f1 else \e' -> f1 e' && f2 e'
                    pass = const True
                in (w, filter f es)
          in map doFilter
        pred = flip DM.lookup (DM.fromList conditions)
  

-- | The result of splitting a variable out of the current unbound
-- variables, in order to bind it.
data NextVar v e u =
  DeadEnd |
  -- ^ There are still unbound variables, but at least one of them has
  -- no possible bindings.
  Done |
  -- ^ There are no more unbound variables.
  NextVar v e [e] u
  -- ^ An unbound variable, its first trial value, a list of other
  -- trial values, and the remaining unbound variables.
  
-- | Tracks trial variable assignments and an arbitrary state
-- of unbound 
data PartialAssignment v e u =
  PartialAssignment {
    nextVar :: u -> NextVar v e u,
    -- ^ Gets the next trial variable, list of bindings, and
    -- reduced unbound variables, if possible.
    effectOfBinding :: v -> e -> u -> u,
    -- ^ Given a variable, a trial value for it, and the remaining
    -- unbound variables, computes new potential bindings for the
    -- remaining variables.
    boundVars :: [BoundVar v e u],
    -- ^ A stack of currently bound variables.
    unboundVars :: u
    -- ^ The current state of the unbound variables.
    }
  
-- | A variable, its current binding, and the bindings we have yet to try.
data BoundVar v e u =
  BoundVar {
    variable :: v,
    -- ^ The variable we're binding.
    value :: e,
    -- ^ The value we're trying.
    remainingValues :: [e],
    -- ^ Values we have yet to try.
    remainingUnboundVars :: u,
    -- ^ The unbound variables after extracting this variable and its
    -- potential bindings. The effect of a new value for this variable
    -- is computed on these unbound variables.
    previousUnboundVars :: u
    -- ^ The unbound variables from before we pushed this bound
    -- variable. This is used to restore the unbound variables when we
    -- pop this bound variable.
    }
             
initial :: (u -> NextVar v e u) -> (v -> e -> u -> u) -> u -> PartialAssignment v e u
initial nextF effectF initialUnboundVars =
  PartialAssignment {
    effectOfBinding = effectF,
    nextVar = nextF,
    boundVars = [],
    unboundVars = initialUnboundVars
    }

-- | Computes the next assignment to the variables, and a succeeding
-- partial assignment.
nextAssignment :: PartialAssignment v e u -> Maybe ([(v,e)], Maybe (PartialAssignment v e u))
nextAssignment p =
  case nextVar p (unboundVars p) of
    DeadEnd ->
      maybe Nothing nextAssignment (nextBinding p)
    Done ->
      Just (assignment p, nextBinding p)
    NextVar v firstVal remainingVals u' ->
      nextAssignment $ pushBinding v firstVal remainingVals u' p

-- | Builds an assignment from the current bound variables.
assignment :: PartialAssignment v e u -> [(v,e)]
assignment = map (\b -> (variable b, value b)) . boundVars
  

-- | Pushes a new bound variable. The bound variable records the
-- variable name, the trial value, the remaining values, and the
-- current unbound state of the partial assignment. The new
-- partial assignment records the binding and its effect on the
-- remaining unbound variables.
pushBinding :: v -> e -> [e] -> u -> PartialAssignment v e u -> PartialAssignment v e u
pushBinding v val remVals remUnboundVars p =
  p { boundVars = toPush:boundVars p,
      unboundVars = effectOfBinding p v val remUnboundVars }
  where toPush = BoundVar { variable = v,
                            value = val,
                            remainingValues = remVals,
                            remainingUnboundVars = remUnboundVars,
                            previousUnboundVars = unboundVars p }
    
-- | Advances to the next trial variable binding. If the top bound
-- variable has values we haven't tried, this changes the trial value
-- and computes the new unbound variables. Otherwise, it pops the top
-- bound variable, and attempts to advance the new top. If the bound
-- variable stack is empty, the result is 'Nothing'.
nextBinding :: PartialAssignment v e u -> Maybe (PartialAssignment v e u)
nextBinding p =
  case boundVars p of
    [] -> Nothing
    (b:rest) ->
      case remainingValues b of
        [] ->
          maybe Nothing nextBinding $ popBinding p
        (e:rem') ->
          Just $ p { boundVars = b':rest,
                     unboundVars = u' }
            where b' = b { value = e,
                           remainingValues = rem' }
                  u' = effectOfBinding p (variable b) e u
                  u  = remainingUnboundVars b

-- | Pops a bound variable. This removes the bound variable and
-- restores the previous unbound variables.
popBinding :: PartialAssignment v e u -> Maybe (PartialAssignment v e u)
popBinding p =
  case boundVars p of
    [] ->
      Nothing
    (toPop:rest) ->
      Just $ p { boundVars = rest, 
                 unboundVars = previousUnboundVars toPop }
