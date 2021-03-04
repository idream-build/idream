module Idream.Command.Fetch
  ( fetchImpl
  , fetchInner
  , PkgMissingInPkgSetErr (..)
  , DisableRefreshErr (..)
  , MissingLocalPathErr (..)
  ) where

import Control.Exception (Exception (..))
import Control.Monad (when)
import Data.Foldable (for_)
import qualified Data.Map as Map
import qualified Data.Text as T
import Idream.App (AppM)
import Idream.Command.Common (PackageGroup, pkgGroupToText, readPkgSetFile, readResolvedProject, reposForGroup)
import Idream.Effects.FileSystem (fsCreateDir, fsDoesDirectoryExist, fsRemovePath)
import Idream.Effects.Git (gitClone, gitFetch, gitReadCurrentBranch, gitReadOriginUrl, gitSwitch)
import Idream.FileLogic (fetchDir, pkgSetFileName)
import Idream.FilePaths (Directory)
import Idream.Types.Common (ProjectName (..), RefreshStrategy (..), RepoName (..))
import Idream.Types.External (GitRepoRef (..), LocalRepoRef (..), RepoRef (..))
import Idream.Types.Internal (ResolvedProject (..))
import LittleLogger (logInfo)
import System.FilePath ((</>))
import UnliftIO.Exception (throwIO)

-- | Top level error type, models all errors that can occur
--   during fetching of dependencies.
newtype PkgMissingInPkgSetErr = PkgMissingInPkgSetErr ProjectName
  deriving (Eq, Show)

instance Exception PkgMissingInPkgSetErr where
  displayException (PkgMissingInPkgSetErr (ProjectName n)) =
    "Package missing in package set: " <> T.unpack n

newtype DisableRefreshErr = DisableRefreshErr RepoName
  deriving (Eq, Show)

instance Exception DisableRefreshErr where
  displayException (DisableRefreshErr (RepoName n)) =
    "Refresh disabled but repo is not usable: " <> T.unpack n

data MissingLocalPathErr = MissingLocalPathErr RepoName Directory
  deriving (Eq, Show)

instance Exception MissingLocalPathErr where
  displayException (MissingLocalPathErr (RepoName n) path) =
    "Missing local path for repo: " <> T.unpack n <> " - " <> path

-- | Top level function that tries to fetch all dependencies.
fetchImpl :: Directory -> PackageGroup -> RefreshStrategy -> AppM ()
fetchImpl projDir group refreshStrat = do
  rp <- readResolvedProject projDir
  logInfo ("Fetching dependencies for project " <> unProjName (rpName rp) <> " with " <> pkgGroupToText group)
  fetchInner projDir rp group refreshStrat

fetchInner :: Directory -> ResolvedProject -> PackageGroup -> RefreshStrategy -> AppM ()
fetchInner projDir rp group refreshStrat = do
  pkgSet <- readPkgSetFile (projDir </> pkgSetFileName)
  let repoRefs = reposForGroup rp pkgSet group
  logInfo "Resolving dependencies"
  fsCreateDir (projDir </> fetchDir)
  for_ (Map.toList repoRefs) $ \(repo, ref) -> do
    fetchRepo projDir refreshStrat repo ref
  logInfo "Finished fetching dependencies"

-- | Fetches a project as specified in the top level package set file.
-- TODO(ejconlon) Recursively fetch repo deps for idream projects
fetchRepo :: Directory -> RefreshStrategy -> RepoName -> RepoRef -> AppM ()
fetchRepo projDir refreshStrat repo ref =
  case ref of
    RepoRefLocal (LocalRepoRef localDir) -> do
      localExists <- fsDoesDirectoryExist (projDir </> localDir)
      if localExists
        then pure ()
        else throwIO (MissingLocalPathErr repo localDir)
    RepoRefGit gitRef -> do
      let repoDir = projDir </> fetchDir </> T.unpack (unRepoName repo)
      gitEnsure repoDir refreshStrat repo gitRef

gitReadCurrentRef :: Directory -> AppM GitRepoRef
gitReadCurrentRef repoDir = do
  url <- gitReadOriginUrl repoDir
  commit <- gitReadCurrentBranch repoDir
  pure (GitRepoRef url commit)

gitEnsure :: Directory -> RefreshStrategy -> RepoName -> GitRepoRef -> AppM ()
gitEnsure repoDir refreshStrat repo desiredRef@(GitRepoRef url commit) = do
  repoExists <- fsDoesDirectoryExist repoDir
  if repoExists
    then do
      curRef <- gitReadCurrentRef repoDir
      case (curRef == desiredRef, refreshStrat) of
        (True, ForceRefresh) -> do
          logInfo ("Fetching " <> commit)
          gitFetch repoDir commit
          logInfo ("Switching " <> commit)
          gitSwitch repoDir commit
        (True, _) -> pure ()
        (False, DisableRefresh) -> throwIO (DisableRefreshErr repo)
        (False, _) -> do
          logInfo ("Re-cloning " <> url <> " at " <> commit)
          fsRemovePath repoDir
          gitClone repoDir url commit
    else do
      when (refreshStrat == DisableRefresh) (throwIO (DisableRefreshErr repo))
      logInfo ("Cloning " <> url <> " at " <> commit)
      gitClone repoDir url commit
