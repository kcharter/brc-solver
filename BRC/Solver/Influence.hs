{-|

Computes the influence relation implied by two-variable constraints.

When distinct variables @x@ and @y@ have a common constraint, a
binding for @x@ limits the possible bindings for @y@, and
vice-versa. We say the two variables /constrain/ one another, and
define a symmetric /constrains/ relation on variables.

If @x@ constrains @y@ and @y@ constrains @z@, then binding a value to
@x@ can influence the possible values of @z@, and vice-versa. We
define the /influences/ relation as the reflexive transitive closure
of /constrains/.

This is important when we're searching for solutions to a set of
two-variable constraints by making trial assignments. If @x@ and @y@
do not influence one another, then we don't need to consider the
impact on @y@ when we select a binding for @x@, since there cannot be
any impact on @y@. In fact, we can split the variables into their
/influences/ equivalence classes, solve each class independently,
and take the product of the individual solutions. -}

module BRC.Solver.Influence (influenceClasses) where

import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.Tree (Gr)
import Data.Graph.Inductive.Query.DFS (scc)
import qualified Data.Map as DM
import qualified Data.IntMap as DIM
import Data.Maybe (mapMaybe)
import qualified Data.Set as DS

import BRC.Solver.ZeroOneTwo

-- | For a list of two-variable constraints, the equivalence classes
-- of the /influences/ relation.
influenceClasses :: (Ord v) => [TwoVar v e] -> [[v]]
influenceClasses twoVars =
  -- It's sufficient to build the 'constrains' graph and find its
  -- connected components.
  map (map nodeToVar) $ scc g 
    where (g, nodeToVar) = constraintGraph twoVars
        
constraintGraph :: (Ord v) => [TwoVar v e] -> (Gr v (), G.Node -> v)
constraintGraph twos = (G.mkGraph lnodes edges, nodeToVar)
  where lnodes = mkLNodes variables
        varToNode = nodeByVar lnodes
        nodeToVar = varByNode lnodes
        edges = concatMap mkEdges twos
        mkEdges (TwoVar v w) | v /= w = let nv = varToNode v
                                            nw = varToNode w
                                        in [(nv,nw,()), (nw,nv,())]
                             | otherwise = let nv = varToNode v
                                           in [(nv,nv,())]
        variables = uniqueVars twos

nodeByVar :: (Ord v) => [G.LNode v] -> v -> G.Node
nodeByVar lnodes = (DM.!) m
  where m = DM.fromList $ map swap lnodes
        swap (x,y) = (y,x)

varByNode :: [G.LNode v] -> G.Node -> v
varByNode lnodes = (DIM.!) m
  where m = DIM.fromList lnodes

mkLNodes :: [v] -> [G.LNode v]
mkLNodes = zipWith mkNode [1..]
  where mkNode n v = (n, v)

uniqueVars :: (Ord v) => [TwoVar v e] -> [v]
uniqueVars = DS.toList . DS.fromList . concatMap vars

vars :: TwoVar v e -> [v]
vars (TwoVar v w) = [v,w]