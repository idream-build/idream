module Idream.Deps
  ( Deps
  , unDeps
  , depsFromEdges
  , depsFromGroups
  , composeDeps
  , revDeps
  , closureDeps
  , unionDeps
  , lookupDeps
  , unionAllDeps
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

newtype Deps k v = Deps { unDeps :: Map k (Set v) } deriving (Eq, Show, Foldable)

depsFromEdges :: (Foldable f, Ord k, Ord v) => f (k, v) -> Deps k v
depsFromEdges = Deps . foldr go Map.empty where
  go (k, v) m = Map.insert k (maybe (Set.singleton v) (Set.insert v) (Map.lookup k m)) m

depsFromGroups :: (Foldable f, Ord k, Ord v) => f (k, Set v) -> Deps k v
depsFromGroups = Deps . foldr go Map.empty where
  go (k, vs) m = Map.insert k (maybe vs (Set.union vs) (Map.lookup k m)) m

composeDeps :: (Ord k, Ord v, Ord w) => Deps k v -> Deps v w -> Deps k w
composeDeps (Deps kvs) (Deps vws) = Deps (Map.fromList (fmap (fmap (foldr go Set.empty)) (Map.toList kvs))) where
  go v ws = maybe ws (Set.union ws) (Map.lookup v vws)

revDeps :: (Ord k, Ord v) => Deps k v -> Deps v k
revDeps = Deps . foldr goK Map.empty . Map.toList . unDeps where
  goK (k, vs) m = foldr (goV k) m (Set.toList vs)
  goV k v m = Map.insert v (maybe (Set.singleton k) (Set.insert k) (Map.lookup v m)) m

-- TODO(ejconlon) This is a pretty slow way to do it, make it better
-- Not a huge deal, as it's only going to recurse as many times as the max dep path length
closureDeps :: Ord k => Deps k k -> Deps k k
closureDeps m =
  let m' = composeDeps m m
  in if m' == m then m' else closureDeps m'

unionDeps :: (Foldable f, Ord k, Ord v) => f k -> Deps k v -> Set v
unionDeps ks (Deps kvs) = foldr go Set.empty ks where
  go k vs = maybe vs (Set.union vs) (Map.lookup k kvs)

lookupDeps :: Ord k => k -> Deps k v -> Set v
lookupDeps k = fromMaybe Set.empty . Map.lookup k . unDeps

unionAllDeps :: (Ord k, Ord v) => Deps k v -> Set v
unionAllDeps d@(Deps kvs) = unionDeps (Map.keys kvs) d
