module Idream.Deps
  ( Deps
  , depsEdges
  , depsKeys
  , depsValues
  , depsVertices
  , depsFromMap
  , depsFromPath
  , depsFromEdges
  , depsFromGroups
  , composeDeps
  , composeAndUnionDeps
  , revDeps
  , closureDeps
  , unionDeps
  , lookupDeps
  , unionAllDeps
  , linearizeDeps
  , restrictDeps
  ) where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude

data Deps k v = Deps
  { depsEdges :: !(Map k (Set v))
  , depsKeys :: !(Set k)
  , depsValues :: !(Set v)
  } deriving (Eq, Show, Foldable)

instance (Ord k, Ord v) => Semigroup (Deps k v) where
  Deps xe xk xv <> Deps ye yk yv = Deps (Map.unionWith Set.union xe ye) (Set.union xk yk) (Set.union xv yv)

instance (Ord k, Ord v) => Monoid (Deps k v) where
  mempty = Deps Map.empty Set.empty Set.empty
  mappend = (<>)

mapSafeInsert :: (Ord k, Ord v) => k -> v -> Map k (Set v) -> Map k (Set v)
mapSafeInsert k v m = Map.insert k (maybe (Set.singleton v) (Set.insert v) (Map.lookup k m)) m

mapSafeUnion :: (Ord k, Ord v) => k -> Set v -> Map k (Set v) -> Map k (Set v)
mapSafeUnion k vs m =
  if Set.null vs
    then m
    else Map.insert k (maybe vs (Set.union vs) (Map.lookup k m)) m

depsVertices :: Ord k => Deps k k -> Set k
depsVertices (Deps _ b c) = Set.union b c

depsFromMap :: (Ord k, Ord v) => Map k (Set v) -> Deps k v
depsFromMap = depsFromGroups . Map.toList

depsFromPath :: (Foldable f, Ord k) => f k -> Deps k k
depsFromPath fs =
  let ks = toList fs
  in case toList fs of
    [] -> mempty
    [k] -> Deps Map.empty Set.empty (Set.singleton k)
    _ -> depsFromEdges (zip ks (tail ks))

depsFromEdges :: (Foldable f, Ord k, Ord v) => f (k, v) -> Deps k v
depsFromEdges edges = Deps finalM finalB finalC where
  (finalM, finalB, finalC) = foldr go (Map.empty, Set.empty, Set.empty) edges
  go (k, v) (m, b, c) =
    let m' = mapSafeInsert k v m
        b' = Set.insert k b
        c' = Set.insert v c
    in (m', b', c')

depsFromGroups :: (Foldable f, Ord k, Ord v) => f (k, Set v) -> Deps k v
depsFromGroups groups = Deps finalM finalB finalC where
  (finalM, finalB, finalC) = foldr go (Map.empty, Set.empty, Set.empty) groups
  go (k, vs) (m, b, c) =
    let m' = mapSafeUnion k vs m
        b' = Set.insert k b
        c' = Set.union vs c
    in (m', b', c')

composeDeps :: (Ord k, Ord v, Ord w) => Deps k v -> Deps v w -> Deps k w
composeDeps (Deps ke kb _) dv@(Deps _ _ vc) = Deps m kb vc where
  m = foldr go Map.empty (Map.toList ke)
  go (k, vs) n = let ws = unionDeps vs dv in if Set.null ws then n else Map.insert k ws n

composeAndUnionDeps :: Ord k => Deps k k -> Deps k k
composeAndUnionDeps d@(Deps m b c) = Deps n b c where
  n = foldr go m (Map.toList m)
  go (k, vs) = mapSafeUnion k (unionDeps vs d)

revDeps :: (Ord k, Ord v) => Deps k v -> Deps v k
revDeps (Deps fwdM fwdB fwdC) = Deps finalBwdM fwdC fwdB where
  finalBwdM = foldr go Map.empty (Map.toList fwdM)
  go (k, vs) m = foldr (`mapSafeInsert` k) m (Set.toList vs)

closureDeps :: Ord k => Deps k k -> Deps k k
closureDeps m =
  let m' = composeAndUnionDeps m
  in if m' == m then m' else closureDeps m'

unionDeps :: (Foldable f, Ord k, Ord v) => f k -> Deps k v -> Set v
unionDeps ks (Deps ke _ _) = foldr go Set.empty ks where
  go k vs = maybe vs (Set.union vs) (Map.lookup k ke)

lookupDeps :: Ord k => k -> Deps k v -> Set v
lookupDeps k (Deps ke _ _) = fromMaybe Set.empty (Map.lookup k ke)

unionAllDeps :: (Ord k, Ord v) => Deps k v -> Set v
unionAllDeps d@(Deps ke _ _) = unionDeps (Map.keys ke) d

data Op k = OpSearch !k | OpOut !k deriving (Eq, Show)

linearizeDeps :: Ord k => Deps k k -> [k]
linearizeDeps d = finalOut where
  Deps m _ _ = revDeps d
  (finalOut, _) = foldr goRoot ([], Set.empty) (Set.toList (depsVertices d))
  goRoot k (out, seen) = goEach out seen [OpSearch k]
  goEach out seen oks =
    case oks of
      [] -> (out, seen)
      (ok:oks') ->
        case ok of
          OpOut k -> goEach (k:out) seen oks'
          OpSearch k ->
            if Set.member k seen
              then goEach out seen oks'
              else let deps = maybe [] (fmap OpSearch . Set.toList) (Map.lookup k m)
                   in goEach out (Set.insert k seen) (deps ++ [OpOut k] ++ oks')

restrictDeps :: Ord k => (k -> Bool) -> Deps k k -> Deps k k
restrictDeps pcate fromDeps@(Deps fromEdges fromKs fromVs) = Deps toEdges toKs toVs where
  initKs = Set.filter pcate (fromKs <> fromVs)
  initVs = unionDeps initKs fromDeps
  finalKs = go initKs initVs
  toEdges = Map.filterWithKey (\k _ -> Set.member k finalKs) fromEdges
  toKs = Set.intersection finalKs fromKs
  -- Those values that are actually (_, v) in the edge map
  actualVs = Map.foldr Set.union Set.empty toEdges
  -- Output values that are either unrepresented from the original value set (i.e. not in keys)
  -- Or they're actual values in the map
  toVs = Set.intersection finalKs (Set.difference fromVs toKs) <> actualVs
  go ks vs =
    let us = Set.difference vs ks
    in if Set.null us then ks else go (ks <> us) (unionDeps us fromDeps)
