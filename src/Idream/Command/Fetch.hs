module Idream.Command.Fetch
  ( fetchDeps
  , PkgMissingInPkgSetErr (..)
  ) where

import Control.Exception (Exception (..))
import Control.Monad (void)
import Data.Foldable (for_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Idream.App (AppM)
import Idream.Command.Common (readPkgFile, readProjFile, PackageGroup, pkgGroupToText, mkPkgGroup, resolveProj, readPkgSetFile, reposForGroup)
import Idream.Effects.FileSystem (fsCreateDir, fsDoesDirectoryExist)
import Idream.Effects.Git (gitEnsure)
import Idream.FileLogic (repoDir, projFileName, pkgSetFileName, fetchDir)
import Idream.FilePaths (Directory)
import Idream.Types.Common (ProjectName (..), PackageName, RepoName (..))
import Idream.Types.External (Project (..), RepoRef (..), LocalRepoRef (..), GitRepoRef (..))
import Idream.Types.Internal (ResolvedProject(..))
import LittleLogger (logDebug, logInfo, logWarning)
import System.FilePath ((</>))
import UnliftIO.Exception (throwIO)
import UnliftIO.IORef (IORef, modifyIORef', newIORef, readIORef)

-- | Helper data type used for marking if a project was already fetched or not.
--   This helps detecting/breaking a cycle in the dependency graph.
data FetchInfo = AlreadyFetched | NewlyFetched
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
fetchDeps :: Directory -> [PackageName] -> AppM ()
fetchDeps projDir pkgNames = do
  let group = mkPkgGroup pkgNames
  proj <- readProjFile (projDir </> projFileName)
  logInfo ("Fetching dependencies for project " <> unProjName (projectName proj) <> " with " <> pkgGroupToText group <> ".")
  resolvedProj <- resolveProj proj
  -- resolve packages, gather all dependendencies, then gather repos
  -- download repos
  if null (rpPackages resolvedProj)
    then logWarning ("Project contains no packages yet, skipping fetch step. "
                    <> "Use `idream add` to add a package to this project first.")
    else do
      pkgSet <- readPkgSetFile (projDir </> pkgSetFileName)
      let repoRefs = reposForGroup resolvedProj pkgSet group
      logInfo "Resolving dependencies"
      fsCreateDir fetchDir
      for_ (Map.toList repoRefs) $ \(repo, ref) -> do
        void (fetchRepo projDir repo ref)
  logInfo "Finished fetching dependencies."

-- | Fetches a project as specified in the top level package set file.
fetchRepo :: Directory -> RepoName -> RepoRef -> AppM ()
fetchRepo projDir repo ref =
  case ref of
    RepoRefLocal (LocalRepoRef localDir) -> do
      localExists <- fsDoesDirectoryExist localDir
      if localExists
        then pure ()
        else error "TODO - fail"
    RepoRefGit (GitRepoRef url commit) -> do
      let repoDir = projDir </> fetchDir </> T.unpack (unRepoName repo)
      gitEnsure repoDir url commit

-- fetchProj (PackageSet pkgs) projName@(ProjectName name) =
--   case Map.lookup name pkgs of
--     Nothing -> throwIO (PkgMissingInPkgSetErr projName)
--     Just (PackageDescr repo version) -> do
--       let repoDir' = repoDir projName
--           projFile = repoDirProjFile projName
--       dirExists <- fsDoesDirectoryExist repoDir'
--       fileExists <- fsDoesFileExist projFile
--       case (dirExists, fileExists) of
--         (True, True) ->
--           AlreadyFetched <$> readProjFile projFile
--         _ -> do
--           gitClone repo repoDir'
--           gitCheckout repo version repoDir'
--           NewlyFetched <$> readProjFile projFile

-- -- | Recursively fetches all dependencies for a project.
-- fetchDepsForProject :: IORef DepGraph -> PackageSet -> Project -> AppM ()
-- fetchDepsForProject graphRef pkgSet (Project projName pkgs) = do
--   logDebug ("Fetching dependencies for project: " <> unProjName projName <> ".")
--   for_ pkgs (fetchDepsForPackage graphRef pkgSet projName)

-- -- | Recursively fetch all dependencies for a package
-- fetchDepsForPackage :: IORef DepGraph -> PackageSet -> ProjectName -> PackageName -> AppM ()
-- fetchDepsForPackage graphRef pkgSet projName pkgName = do
--   logDebug ("Fetching dependencies for package: " <> toText pkgName <> ".")
--   pkgDeps <- readPkgDeps projName pkgName
--   let isOld (AlreadyFetched _) = True
--       isOld _ = False
--       unwrap (AlreadyFetched proj) = proj
--       unwrap (NewlyFetched proj) = proj
--       projNames = nub $ depProjName <$> pkgDeps
--   (oldSubProjects, newSubProjects) <- partition isOld <$> mapM (fetchProj pkgSet) projNames
--   let oldSubProjects' = unwrap <$> oldSubProjects
--       newSubProjects' = unwrap <$> newSubProjects
--       subProjects = oldSubProjects' ++ newSubProjects'
--   modifyIORef' graphRef (updateGraph (DepNode pkgName projName) subProjects)
--   for_ newSubProjects' (fetchDepsForProject graphRef pkgSet)

-- -- | Fetches a project as specified in the top level package set file.
-- fetchProj :: PackageSet -> ProjectName -> AppM FetchInfo
-- fetchProj (PackageSet pkgs) projName@(ProjectName name) =
--   case Map.lookup name pkgs of
--     Nothing -> throwIO (PkgMissingInPkgSetErr projName)
--     Just (PackageDescr repo version) -> do
--       let repoDir' = repoDir projName
--           projFile = repoDirProjFile projName
--       dirExists <- fsDoesDirectoryExist repoDir'
--       fileExists <- fsDoesFileExist projFile
--       case (dirExists, fileExists) of
--         (True, True) ->
--           AlreadyFetched <$> readProjFile projFile
--         _ -> do
--           gitClone repo repoDir'
--           gitCheckout repo version repoDir'
--           NewlyFetched <$> readProjFile projFile

-- -- | Reads the package file to determine the project dependencies.
-- readPkgDeps ::  ProjectName -> PackageName -> AppM [Dependency]
-- readPkgDeps projName pkgName = do
--   pkgFilePath <- getPkgFilePath pkgName projName
--   (Package _ _ _ deps) <- readPkgFile pkgFilePath
--   return deps
