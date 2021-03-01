module Idream.Deps
  ( Deps
  , mkDeps
  , unDeps
  , depsFromPath
  , depsFromEdges
  , depsFromGroups
  , composeDeps
  , revDeps
  , closureDeps
  , unionDeps
  , lookupDeps
  , unionAllDeps
  , linearizeDeps
  , filterDeps
  ) where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

newtype Deps k v = Deps { unDeps :: Map k (Set v) } deriving (Eq, Show, Foldable)

instance (Ord k, Ord v) => Semigroup (Deps k v) where
  Deps x <> Deps y = Deps (Map.unionWith Set.union x y)

instance (Ord k, Ord v) => Monoid (Deps k v) where
  mempty = Deps Map.empty
  mappend = (<>)

mkDeps :: Map k (Set v) -> Deps k v
mkDeps = Deps . Map.filter (not . Set.null)

depsFromPath :: (Foldable f, Ord k) => f k -> Deps k k
depsFromPath fs =
  let ks = toList fs
  in depsFromEdges (zip ks (tail ks))

depsFromEdges :: (Foldable f, Ord k, Ord v) => f (k, v) -> Deps k v
depsFromEdges = Deps . foldr go Map.empty where
  go (k, v) m = Map.insert k (maybe (Set.singleton v) (Set.insert v) (Map.lookup k m)) m

depsFromGroups :: (Foldable f, Ord k, Ord v) => f (k, Set v) -> Deps k v
depsFromGroups = Deps . foldr go Map.empty where
  go (k, vs) m = Map.insert k (maybe vs (Set.union vs) (Map.lookup k m)) m

composeDeps :: (Ord k, Ord v, Ord w) => Deps k v -> Deps v w -> Deps k w
composeDeps (Deps kvs) (Deps vws) = mkDeps (Map.fromList (fmap (fmap (foldr go Set.empty)) (Map.toList kvs))) where
  go v ws = maybe ws (Set.union ws) (Map.lookup v vws)

revDeps :: (Ord k, Ord v) => Deps k v -> Deps v k
revDeps = Deps . foldr goK Map.empty . Map.toList . unDeps where
  goK (k, vs) m = foldr (goV k) m (Set.toList vs)
  goV k v m = Map.insert v (maybe (Set.singleton k) (Set.insert k) (Map.lookup v m)) m

-- TODO(ejconlon) This is a pretty slow way to do it, make it better
-- Not a huge deal, as it's only going to recurse as many times as the max dep path length
closureDeps :: Ord k => Deps k k -> Deps k k
closureDeps m =
  let m' = m <> composeDeps m m
  in if m' == m then m' else closureDeps m'

unionDeps :: (Foldable f, Ord k, Ord v) => f k -> Deps k v -> Set v
unionDeps ks (Deps kvs) = foldr go Set.empty ks where
  go k vs = maybe vs (Set.union vs) (Map.lookup k kvs)

lookupDeps :: Ord k => k -> Deps k v -> Set v
lookupDeps k = fromMaybe Set.empty . Map.lookup k . unDeps

unionAllDeps :: (Ord k, Ord v) => Deps k v -> Set v
unionAllDeps d@(Deps kvs) = unionDeps (Map.keys kvs) d

data Op k = OpSearch !k | OpOut !k deriving (Eq, Show)

linearizeDeps :: Ord k => Deps k k -> [k]
linearizeDeps d@(Deps n) = finalOut where
  Deps m = revDeps d
  roots = Map.keysSet n <> Map.keysSet m
  (finalOut, _) = foldr goRoot ([], Set.empty) (Set.toList roots)
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

filterDeps :: Ord k => (k -> Bool) -> Deps k k -> (Deps k k, Set k)
filterDeps pcate initDeps@(Deps m) = (finalDeps, finalKs) where
  initKs = Set.filter pcate (Map.keysSet m)
  initVs = unionDeps initKs initDeps
  finalKs = go initKs initVs
  finalDeps = Deps (Map.filterWithKey (\k _ -> Set.member k finalKs) m)
  go ks vs =
    let us = Set.difference vs ks
    in if Set.null us then ks else go (ks <> us) (unionDeps us initDeps)
