module Idream.Command.Fetch
  ( fetchDeps
  , PkgMissingInPkgSetErr (..)
  ) where

import Control.Exception (Exception (..))
import Data.Foldable (for_)
import Data.List (nub, partition)
import qualified Data.Map as Map
import qualified Data.Text as T
import Idream.App (AppM)
import Idream.Command.Common (getPkgFilePath, readPkgFile, readProjFile, readRootPkgSetFile, readRootProjFile,
                              setupBuildDir)
import Idream.Effects.FileSystem (fsDoesDirectoryExist, fsDoesFileExist)
import Idream.Effects.Git (gitCheckout, gitClone)
import Idream.FileLogic (depGraphFile, repoDir, repoDirProjFile)
import Idream.Graph (DepGraph, DepNode (..), mkGraphFromProject, saveGraphToJSON, updateGraph)
import Idream.ToText (ToText (..))
import Idream.Types (Dependency (..), Package (..), PackageDescr (..), PackageName, PackageSet (..), Project (..),
                     ProjectName (..))
import LittleLogger (logDebug, logInfo)
import UnliftIO.Exception (throwIO)
import UnliftIO.IORef (IORef, modifyIORef', newIORef, readIORef)

-- | Helper data type used for marking if a project was already fetched or not.
--   This helps detecting/breaking a cycle in the dependency graph.
data FetchInfo =
    AlreadyFetched Project
  | NewlyFetched Project
  deriving (Eq, Show)

-- | Top level error type, models all errors that can occur
--   during fetching of dependencies.
newtype PkgMissingInPkgSetErr = PkgMissingInPkgSetErr ProjectName
  deriving (Eq, Show)

instance Exception PkgMissingInPkgSetErr where
  displayException (PkgMissingInPkgSetErr (ProjectName n)) =
    "Package missing in package set: " <> T.unpack n <> "."

-- TODO add force fetch flag

-- | Top level function that tries to fetch all dependencies.
fetchDeps :: AppM ()
fetchDeps = do
  setupBuildDir
  rootProj@(Project projName rootPkgs) <- readRootProjFile
  if null rootPkgs
    then logInfo ("Project contains no packages yet, skipping fetch step. "
                 <> "Use `idream add` to add a package to this project first.")
    else do
      pkgSet <- readRootPkgSetFile
      logInfo ("Fetching dependencies for " <> toText projName <> ".")
      let initialGraph = mkGraphFromProject rootProj
      graphRef <- newIORef initialGraph
      fetchDepsForProject graphRef pkgSet rootProj
      finalGraph <- readIORef graphRef
      saveGraphToJSON depGraphFile finalGraph
  logInfo "Finished fetching dependencies."

-- | Recursively fetches all dependencies for a project.
fetchDepsForProject :: IORef DepGraph -> PackageSet -> Project -> AppM ()
fetchDepsForProject graphRef pkgSet (Project projName pkgs) = do
  logDebug ("Fetching dependencies for project: " <> unProjName projName <> ".")
  for_ pkgs (fetchDepsForPackage graphRef pkgSet projName)

-- | Recursively fetch all dependencies for a package
fetchDepsForPackage :: IORef DepGraph -> PackageSet -> ProjectName -> PackageName -> AppM ()
fetchDepsForPackage graphRef pkgSet projName pkgName = do
  logDebug ("Fetching dependencies for package: " <> toText pkgName <> ".")
  pkgDeps <- readPkgDeps projName pkgName
  let isOld (AlreadyFetched _) = True
      isOld _ = False
      unwrap (AlreadyFetched proj) = proj
      unwrap (NewlyFetched proj) = proj
      projNames = nub $ depProjName <$> pkgDeps
  (oldSubProjects, newSubProjects) <- partition isOld <$> mapM (fetchProj pkgSet) projNames
  let oldSubProjects' = unwrap <$> oldSubProjects
      newSubProjects' = unwrap <$> newSubProjects
      subProjects = oldSubProjects' ++ newSubProjects'
  modifyIORef' graphRef (updateGraph (DepNode pkgName projName) subProjects)
  for_ newSubProjects' (fetchDepsForProject graphRef pkgSet)

-- | Fetches a project as specified in the top level package set file.
fetchProj :: PackageSet -> ProjectName -> AppM FetchInfo
fetchProj (PackageSet pkgs) projName@(ProjectName name) =
  case Map.lookup name pkgs of
    Nothing -> throwIO (PkgMissingInPkgSetErr projName)
    Just (PackageDescr repo version) -> do
      let repoDir' = repoDir projName
          projFile = repoDirProjFile projName
      dirExists <- fsDoesDirectoryExist repoDir'
      fileExists <- fsDoesFileExist projFile
      case (dirExists, fileExists) of
        (True, True) ->
          AlreadyFetched <$> readProjFile projFile
        _ -> do
          gitClone repo repoDir'
          gitCheckout repo version repoDir'
          NewlyFetched <$> readProjFile projFile

-- | Reads the package file to determine the project dependencies.
readPkgDeps ::  ProjectName -> PackageName -> AppM [Dependency]
readPkgDeps projName pkgName = do
  pkgFilePath <- getPkgFilePath pkgName projName
  (Package _ _ _ deps) <- readPkgFile pkgFilePath
  return deps
