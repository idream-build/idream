
{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveFoldable #-}

module Idream.Graph ( DepGraph
                    , DepNode(..)
                    , GraphErr(..)
                    , Max(..)
                    , MonoidMap(..)
                    , Depth
                    , Phase
                    , BuildPlan(..)
                    , mkGraphFromProject
                    , updateGraph
                    , getLeafNodes
                    , mkBuildPlan
                    , createBuildPlan
                    , saveGraphToJSON
                    , loadGraphFromJSON
                    , handleGraphErr
                    ) where

-- Imports

import Idream.Types ( Project(..), ProjectName(..), PackageName(..) )
import Idream.Command.Common ( tryAction )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map ( Map )
import Data.Set ( Set )
import Data.Monoid
import Data.Aeson
import Data.Aeson.Text ( encodeToLazyText )
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Algebra.Graph as Graph
import qualified Algebra.Graph.AdjacencyMap as AM
import Control.Monad.Except
import Idream.Log ( MonadLogger )
import qualified Idream.Log as Log
import Control.Monad.State
import Control.Exception ( IOException )


-- Data types

-- | Type used in each node of the graph, it combines the package name
--   together with the project in which it belongs.
data DepNode = DepNode PackageName ProjectName
             deriving (Eq, Ord, Show)

-- | Type representing the dependency graph.
type DepGraph = Graph.Graph DepNode

-- | Type containing possible errors that can occur while working with graphs.
data GraphErr = SaveGraphErr IOException
              | LoadGraphErr IOException
              | ParseGraphErr String
              deriving (Eq, Show)

-- | Data type used for (de-)serializing graphs.
data GraphInfo = GraphInfo [DepNode] [(DepNode, DepNode)]
               deriving (Eq, Show)

-- | Type representing how deep a node is located in a graph.
type Depth = Int

-- | Type representing an adjacency map of a graph.
type AdjacencyMap a = Map a (Set a)

-- | Monoid used for finding the maximum.
newtype Max a = Max a deriving (Eq, Ord, Show)

-- | Wrapper type around map that combines values using monoidal appends.
newtype MonoidMap k v = MonoidMap (Map k v) deriving (Eq, Show)

-- | Type alias for a phase in the build plan.
type Phase = Int

-- | Type used for describing the build plan idream uses.
data BuildPlan a = BuildPlan { numPhases :: Phase
                             , phaseMap :: Map Phase (Set a)
                             } deriving (Eq, Show, Foldable)

-- | Helper data type for finding the depth of each node in a graph.
data TraverseState a = TraverseState Depth (MonoidMap a (Max Depth))
                     deriving (Eq, Show)


-- Instances

instance FromJSON DepNode where
  parseJSON (Object o) = DepNode
                      <$> o .: "package_name"
                      <*> o .: "project_name"
  parseJSON _ = mzero

instance ToJSON DepNode where
  toJSON (DepNode pkgName projName) =
    object [ "package_name" .= pkgName
           , "project_name" .= projName
           ]

instance FromJSON GraphInfo where
  parseJSON (Object o) = GraphInfo
                      <$> o .: "vertices"
                      <*> o .: "edges"
  parseJSON _ = mzero

instance ToJSON GraphInfo where
  toJSON (GraphInfo vs es) =
    object [ "vertices" .= vs
           , "edges" .= es
           ]

instance (Ord a, Num a) => Monoid (Max a) where
  mempty = Max 0
  mappend (Max a) (Max b) = if a > b then Max a else Max b

instance (Ord k, Monoid v) => Monoid (MonoidMap k v) where
  mempty = MonoidMap Map.empty
  mappend (MonoidMap a) (MonoidMap b) = MonoidMap (Map.unionWith mappend a b)


-- Functions

-- | Constructs an initial graph from a top level project.
mkGraphFromProject :: Project -> DepGraph
mkGraphFromProject p = Graph.overlays (Graph.vertex <$> nodesFromProject p)

-- | Updates the dependency graph by adding a new list of projects to a package.
--   Each project can itself contain further dependencies.
updateGraph :: DepNode -> [Project] -> DepGraph -> DepGraph
updateGraph node projects g = mergedGraph where
  mergedGraph :: DepGraph
  mergedGraph = Graph.simplify . Graph.overlays $ g:projectGraphs
  projectGraphs :: [DepGraph]
  projectGraphs = connectProjectToGraph <$> projects
  connectProjectToGraph :: Project -> DepGraph
  connectProjectToGraph p = Graph.star node (nodesFromProject p)

-- | Creates a list of nodes to be inserted into a dependency graph.
nodesFromProject :: Project -> [DepNode]
nodesFromProject (Project projName deps) = flip DepNode projName <$> deps

-- | Converts a graph to an adjacency map.
--   Each key (node in a graph) maps onto a set of direct neighbours.
--   NOTE: the neighbours are only in the direction of the arrows in the graph.
toAdjacencyMap :: Ord a => Graph.Graph a -> AdjacencyMap a
toAdjacencyMap g =
  let vs = Graph.vertexList g
      es = Graph.edgeList g
  in AM.adjacencyMap $ AM.graph vs es

-- | Traverses a graph starting from a specific node
--   and keeps a track of max depth for each node along the way.
traverseGraphWithDepth :: Ord a
                       => Graph.Graph a
                       -> a
                       -> MonoidMap a (Max Depth)
traverseGraphWithDepth g v =
  let am = toAdjacencyMap g
      beginState = TraverseState 0 mempty
      traverseAMWithDepth' = traverseAMWithDepth am v
      (TraverseState _ endState) = execState traverseAMWithDepth' beginState
  in endState

-- | Traverse an adjacency map in order to find the max depth of each node
--   in a graph, starting from a specific node.
traverseAMWithDepth :: Ord a
                    => AdjacencyMap a
                    -> a
                    -> State (TraverseState a) ()
traverseAMWithDepth am v =
  let singleton key val = MonoidMap (Map.singleton key (Max val))
  in case Map.lookup v am of
    Nothing -> return ()  -- node not located in this graph
    Just vs -> do
      -- first increase depth by 1 level, adding current node,
      -- then go recursively through each of the neighbours
      -- and reset depth back to current level
      modify $ \(TraverseState d m) -> TraverseState (d + 1) (m <> singleton v d)
      mapM_ (traverseAMWithDepth am) vs
      modify $ \(TraverseState d m) -> TraverseState (d - 1) m

-- | Finds all nodes in the graph that have no outgoing arrows.
getLeafNodes :: Ord a => Graph.Graph a -> [a]
getLeafNodes g =
  let am = toAdjacencyMap g
      vs = Graph.vertexList g
      isLeafNode Nothing = False  -- not located in graph
      isLeafNode (Just set) = set == mempty
  in [v | v <- vs, isLeafNode $ Map.lookup v am]

mkBuildPlan :: Ord a => MonoidMap a (Max Depth) -> BuildPlan a
mkBuildPlan (MonoidMap depthMap) =
  let mapList = Map.toList $ fmap (\(Max x) -> x) depthMap
      mapList' = [(d, Set.singleton a) | (a, d) <- mapList]
      mapSet = Map.fromListWith Set.union mapList'
      phases = length mapSet
  in BuildPlan phases mapSet

  -- | Creates a build plan, given a graph.
createBuildPlan :: Ord a => Graph.Graph a -> BuildPlan a
createBuildPlan g =
  let leafs = getLeafNodes g
      g' = Graph.transpose g
      depthMaps = map (traverseGraphWithDepth g') leafs
      depthMap = mconcat depthMaps
  in mkBuildPlan depthMap

-- | Converts the graph to a different representation.
toGraphInfo :: DepGraph -> GraphInfo
toGraphInfo g = GraphInfo (Graph.vertexList g) (Graph.edgeList g)

-- | Creates an algebraic graph based on a GraphInfo structure.
fromGraphInfo :: GraphInfo -> DepGraph
fromGraphInfo (GraphInfo vs es) = Graph.graph vs es

-- | Saves a graph to a JSON file.
saveGraphToJSON :: (MonadError GraphErr m, MonadIO m)
                => FilePath -> DepGraph -> m ()
saveGraphToJSON file =
  tryAction SaveGraphErr . TIO.writeFile file . encodeToLazyText . toGraphInfo

-- | Loads a graph from JSON.
loadGraphFromJSON :: (MonadError GraphErr m, MonadIO m)
                  => FilePath -> m DepGraph
loadGraphFromJSON file = do
  contents <- tryAction LoadGraphErr $ TIO.readFile file
  case eitherDecode' $ encodeUtf8 contents of
    Left err -> throwError $ ParseGraphErr err
    Right graphInfo -> return $ fromGraphInfo graphInfo

-- | Helper function for handling errors related to saving
--   of the dependency graph to a file.
handleGraphErr :: MonadLogger m => GraphErr -> m ()
handleGraphErr (LoadGraphErr err) =
  Log.err (T.pack $ "Failed to load graph from a file: " <> show err <> ".")
handleGraphErr (SaveGraphErr err) =
  Log.err (T.pack $ "Failed to save graph to a file: " <> show err <> ".")
handleGraphErr (ParseGraphErr err) =
  Log.err (T.pack $ "Failed to parse graph from file: " <> show err <> ".")
