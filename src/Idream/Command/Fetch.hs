module Idream.Command.Fetch
  ( fetchDeps
  , PkgMissingInPkgSetErr (..)
  , NoNetworkErr (..)
  , MissingLocalPathErr (..)
  , Network (..)
  ) where

import Control.Exception (Exception (..))
import Control.Monad (when)
import Data.Foldable (for_)
import qualified Data.Map as Map
import qualified Data.Text as T
import Idream.App (AppM)
import Idream.Command.Common (PackageGroup, pkgGroupToText, readPkgSetFile, reposForGroup, withResolvedProject)
import Idream.Effects.FileSystem (fsCreateDir, fsDoesDirectoryExist, fsRemovePath)
import Idream.Effects.Git (gitClone, gitFetch, gitReadCurrentBranch, gitReadOriginUrl, gitSwitch)
import Idream.FileLogic (fetchDir, pkgSetFileName)
import Idream.FilePaths (Directory)
import Idream.Types.Common (ProjectName (..), RepoName (..))
import Idream.Types.External (GitRepoRef (..), LocalRepoRef (..), RepoRef (..))
import Idream.Types.Internal (ResolvedProject (..))
import LittleLogger (logInfo)
import System.FilePath ((</>))
import UnliftIO.Exception (throwIO)

-- | Whether to use the network
data Network = YesNetwork | AvoidNetwork | NoNetwork deriving (Eq, Show)

-- | Top level error type, models all errors that can occur
--   during fetching of dependencies.
newtype PkgMissingInPkgSetErr = PkgMissingInPkgSetErr ProjectName
  deriving (Eq, Show)

instance Exception PkgMissingInPkgSetErr where
  displayException (PkgMissingInPkgSetErr (ProjectName n)) =
    "Package missing in package set: " <> T.unpack n <> "."

newtype NoNetworkErr = NoNetworkErr RepoName
  deriving (Eq, Show)

instance Exception NoNetworkErr where
  displayException (NoNetworkErr (RepoName n)) =
    "Network usage disabled but could not refresh repo: " <> T.unpack n

data MissingLocalPathErr = MissingLocalPathErr RepoName Directory
  deriving (Eq, Show)

instance Exception MissingLocalPathErr where
  displayException (MissingLocalPathErr (RepoName n) path) =
    "Missing local path for repo: " <> T.unpack n <> " - " <> path

-- | Top level function that tries to fetch all dependencies.
fetchDeps :: Directory -> Network -> PackageGroup -> AppM ()
fetchDeps projDir network group = do
  withResolvedProject "fetch" projDir $ \rp -> do
    logInfo ("Fetching dependencies for project " <> unProjName (rpName rp) <> " with " <> pkgGroupToText group <> ".")
    pkgSet <- readPkgSetFile (projDir </> pkgSetFileName)
    let repoRefs = reposForGroup rp pkgSet group
    logInfo "Resolving dependencies"
    fsCreateDir fetchDir
    for_ (Map.toList repoRefs) $ \(repo, ref) -> do
      fetchRepo projDir network repo ref
  logInfo "Finished fetching dependencies."

-- | Fetches a project as specified in the top level package set file.
-- TODO(ejconlon) Recursively fetch repo deps for idream projects
fetchRepo :: Directory -> Network -> RepoName -> RepoRef -> AppM ()
fetchRepo projDir network repo ref =
  case ref of
    RepoRefLocal (LocalRepoRef localDir) -> do
      localExists <- fsDoesDirectoryExist localDir
      if localExists
        then pure ()
        else throwIO (MissingLocalPathErr repo localDir)
    RepoRefGit gitRef -> do
      let repoDir = projDir </> fetchDir </> T.unpack (unRepoName repo)
      gitEnsure repoDir network repo gitRef

gitReadCurrentRef :: Directory -> AppM GitRepoRef
gitReadCurrentRef repoDir = do
  url <- gitReadOriginUrl repoDir
  commit <- gitReadCurrentBranch repoDir
  pure (GitRepoRef url commit)

gitEnsure :: Directory -> Network -> RepoName -> GitRepoRef -> AppM ()
gitEnsure repoDir network repo desiredRef@(GitRepoRef url commit) = do
  repoExists <- fsDoesDirectoryExist repoDir
  if repoExists
    then do
      curRef <- gitReadCurrentRef repoDir
      case (curRef == desiredRef, network) of
        (True, YesNetwork) -> do
          logInfo ("Fetching " <> commit)
          gitFetch repoDir commit
          logInfo ("Switching " <> commit)
          gitSwitch repoDir commit
        (True, _) -> pure ()
        (False, NoNetwork) -> throwIO (NoNetworkErr repo)
        (False, _) -> do
          logInfo ("Re-cloning " <> url <> " at " <> commit)
          fsRemovePath repoDir
          gitClone repoDir url commit
    else do
      when (network == NoNetwork) (throwIO (NoNetworkErr repo))
      logInfo ("Cloning " <> url <> " at " <> commit)
      gitClone repoDir url commit
