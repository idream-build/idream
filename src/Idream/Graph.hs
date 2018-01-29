
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Idream.Graph ( DepGraph
                    , GraphErr(..)
                    , emptyGraph
                    , updateGraph
                    , saveGraphToJSON
                    , loadGraphFromJSON
                    ) where

-- Imports

import Idream.Types ( Project(..), PackageName(..) )
import Idream.Command.Common ( tryAction )
import Data.Aeson
import Data.Aeson.Text ( encodeToLazyText )
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import qualified Data.Text.Lazy.IO as TIO
import qualified Algebra.Graph as Graph
import Control.Monad.Except
import Control.Exception ( IOException )


-- Data types

type DepGraph = Graph.Graph PackageName
data GraphErr = SaveGraphErr IOException
              | LoadGraphErr IOException
              | ParseGraphErr String
              deriving (Eq, Show)
data GraphInfo = GraphInfo [PackageName] [(PackageName, PackageName)]
               deriving (Eq, Show)


-- Instances

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


-- Functions

-- | Constructs an empty graph.
emptyGraph :: DepGraph
emptyGraph = Graph.empty

-- | Updates the dependency graph by adding a new list of projects to it.
--   Each project can itself contain further dependencies.
updateGraph :: [Project] -> DepGraph -> DepGraph
updateGraph xs g = mergedGraph where
  mkProjectGraph :: Project -> DepGraph
  mkProjectGraph (Project pkgName deps) = Graph.star pkgName deps
  projectGraphs :: [DepGraph]
  projectGraphs = fmap mkProjectGraph xs
  mergedGraph :: DepGraph
  mergedGraph = Graph.simplify . Graph.overlays $ g:projectGraphs

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
