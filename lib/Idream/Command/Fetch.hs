
module Idream.Command.Fetch ( fetchDeps ) where


-- Imports

import Control.Monad.State
import qualified Idream.Log as Log
import Idream.Log ( MonadLogger )
import Idream.SafeIO
import Control.Exception ( IOException )
import System.Process ( createProcess, waitForProcess, cwd, proc )
import System.Exit ( ExitCode(..) )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Monoid ( (<>) )
import Data.Aeson ( eitherDecode )
import Data.List ( partition )
import qualified Data.Text as T
import Idream.Command.Common ( setupBuildDir, readRootProjFile, readProjFile

                             , readPkgFile, checkDirExists, checkFileExists
                             , getPkgFilePath, handleReadProjectErr, handleReadPkgErr
                             , ReadProjectErr(..), ReadPkgErr(..) )
import Idream.Types
import Idream.Graph
import Idream.FileSystem


-- Types

-- | Helper data type used for marking if a project was already fetched or not.
--   This helps detecting/breaking a cycle in the dependency graph.
data FetchInfo = AlreadyFetched Project
               | NewlyFetched Project
               deriving (Eq, Show)

-- | Top level error type, models all errors that can occur
--   during fetching of dependencies.
data FetchErr = FSetupBuildDirErr IOException
              | FReadProjectErr ReadProjectErr
              | FReadPkgSetErr ReadPkgSetErr
              | FFetchPkgErr FetchPkgErr
              | FGraphErr GraphErr
              deriving (Eq, Show)

-- | Error type used for describing errors that can occur while reading out package set file.
data ReadPkgSetErr = PkgSetFileNotFound IOException
                   | PkgSetParseErr String
                   deriving (Eq, Show)

-- | Error type used for describing errors that can occur during clone/checkout of dependencies.
data GitCloneErr = CloneFailError Repo IOException
                 | CloneFailExitCode Repo ExitCode
                 | CheckoutFailError Repo Version IOException
                 | CheckoutFailExitCode Repo Version ExitCode
                 deriving (Eq, Show)

-- | Error type used for describing errors when fetching packages.
data FetchPkgErr = FPPkgMissingInPkgSet ProjectName
                 | FPReadPkgDepsErr ReadPkgErr
                 | FPReadProjectErr ReadProjectErr
                 | FPGitCloneErr GitCloneErr
                 | FPReadDirErr IOException
                 | FPReadFileErr IOException
                 deriving (Eq, Show)


-- Functions

-- TODO add force fetch flag


-- | Top level function that tries to fetch all dependencies.
fetchDeps :: ( MonadLogger m, MonadIO m ) => m ()
fetchDeps = do
  result <- runSafeIO $ do
    projFileExists <- checkFileExists' FFetchPkgErr projectFile
    if not projFileExists
      then Log.err "Did not find project file, aborting."
      else fetchDeps'
  case result of
    Left err -> handleFetchErr err
    Right _ -> Log.info "Finished fetching dependencies."

-- | Helper function that does the actual fetching of dependencies.
fetchDeps' :: ( MonadLogger m, MonadSafeIO FetchErr m ) => m ()
fetchDeps' = do
  setupBuildDir FSetupBuildDirErr
  rootProj@(Project projName rootPkgs) <- readRootProjFile FReadProjectErr
  if null rootPkgs
    then Log.info ("Project contains no packages yet, skipping fetch step. "
                <> "Use `idream add` to add a package to this project first.")
    else do
      pkgSet <- readPkgSetFile FReadPkgSetErr
      Log.info ("Fetching dependencies for " <> unProjName projName <> ".")
      let initialGraph = mkGraphFromProject rootProj
          fetchDepsForProj = fetchDepsForProject FFetchPkgErr pkgSet rootProj
      graph <- execStateT fetchDepsForProj initialGraph
      saveGraphToJSON FGraphErr depGraphFile graph


-- | Recursively fetches all dependencies for a project.
fetchDepsForProject :: ( MonadState DepGraph m
                       , MonadLogger m
                       , MonadSafeIO e m )
                    => (FetchPkgErr -> e)
                    -> PackageSet -> Project -> m ()
fetchDepsForProject f pkgSet (Project projName pkgs) = do
  Log.debug ("Fetching dependencies for project: " <> unProjName projName <> ".")
  mapM_ (fetchDepsForPackage f pkgSet projName) pkgs

-- | Recursively fetch all dependencies for a package
fetchDepsForPackage :: ( MonadState DepGraph m
                       , MonadLogger m
                       , MonadSafeIO e m )
                    => (FetchPkgErr -> e)
                    -> PackageSet -> ProjectName -> PackageName -> m ()
fetchDepsForPackage f pkgSet projName pkgName = do
  Log.debug ("Fetching dependencies for package: " <> unPkgName pkgName <> ".")
  pkgDeps <- readPkgDeps f projName pkgName
  let isOld (AlreadyFetched _) = True
      isOld _ = False
  (oldSubProjects, newSubProjects) <- partition isOld <$> mapM (fetchProj f pkgSet) pkgDeps
  let unwrap (AlreadyFetched proj) = proj
      unwrap (NewlyFetched proj) = proj
      oldSubProjects' = unwrap <$> oldSubProjects
      newSubProjects' = unwrap <$> newSubProjects
      subProjects = oldSubProjects' ++ newSubProjects'
  modify $ updateGraph (DepNode pkgName projName) subProjects
  mapM_ (fetchDepsForProject f pkgSet) newSubProjects'

-- | Fetches a project as specified in the top level package set file.
fetchProj :: ( MonadLogger m, MonadSafeIO e m )
          => (FetchPkgErr -> e)
          -> PackageSet -> ProjectName -> m FetchInfo
fetchProj f (PackageSet pkgs) projName@(ProjectName name) =
  case Map.lookup name pkgs of
    Nothing -> raiseError . f $ FPPkgMissingInPkgSet projName
    Just (PackageDescr repo version) -> do
      let repoDir' = repoDir projName
          projFile = repoDirProjFile projName
      (dirExists, fileExists) <- liftM2 (,) (checkDirExists' f $ repoDir') (checkFileExists' f projFile)
      case (dirExists, fileExists) of
        (True, True) ->
          AlreadyFetched <$> readProjFile (f . FPReadProjectErr) projFile
        _ -> do
          gitClone (f . FPGitCloneErr) repo version repoDir'
          result <- readProjFile (f . FPReadProjectErr) projFile
          return $ NewlyFetched result

-- | Reads the package file to determine the project dependencies.
readPkgDeps :: MonadSafeIO e m
            => (FetchPkgErr -> e)
            -> ProjectName -> PackageName -> m [ProjectName]
readPkgDeps f projName pkgName = do
  pkgFilePath <- getPkgFilePath (f . FPReadProjectErr) pkgName projName
  (Package _ _ _ deps) <- readPkgFile (f . FPReadPkgDepsErr) pkgFilePath
  return deps

-- | Reads out the package set file (idr-package-set.json).
readPkgSetFile :: MonadSafeIO e m => (ReadPkgSetErr -> e) -> m PackageSet
readPkgSetFile f = do
  pkgSetJSON <- liftSafeIO (f . PkgSetFileNotFound) $ BSL.readFile pkgSetFile
  let result = eitherDecode pkgSetJSON
  either (raiseError . f . PkgSetParseErr) return result

-- | Clones a git repo, along with all it's submodules into a directory.
gitClone :: ( MonadLogger m, MonadSafeIO e m )
         => (GitCloneErr -> e) -> Repo -> Version -> Directory -> m ()
gitClone f r@(Repo repo) v@(Version vsn) downloadDir =
  let cloneArgs = ["clone", "--quiet", "--recurse-submodules", T.unpack repo, downloadDir]
      checkoutArgs = ["checkout", "--quiet", T.unpack vsn]
      execProcess f' command cmdArgs maybeDir = liftSafeIO f' $ do
        (_, _, _, procHandle) <- createProcess (proc command cmdArgs) { cwd = maybeDir }
        waitForProcess procHandle
  in do
    Log.debug ("Fetching repo @ " <> repo <> ".")
    cloneResult <- execProcess (f . CloneFailError r) "git" cloneArgs Nothing
    when (cloneResult /= ExitSuccess) $ raiseError . f $ CloneFailExitCode r cloneResult
    checkoutResult <- execProcess (f . CheckoutFailError r v) "git" checkoutArgs (Just downloadDir)
    when (checkoutResult /= ExitSuccess) $ raiseError . f $ CheckoutFailExitCode r v checkoutResult

checkDirExists' :: MonadSafeIO e m => (FetchPkgErr -> e) -> FilePath -> m Bool
checkDirExists' f = checkDirExists (f . FPReadDirErr)

checkFileExists' :: MonadSafeIO e m => (FetchPkgErr -> e) -> FilePath -> m Bool
checkFileExists' f = checkFileExists (f . FPReadFileErr)

-- | Helper function for handling errors that can occur during
--   fetching of dependencies.
handleFetchErr :: MonadLogger m => FetchErr -> m ()
handleFetchErr (FSetupBuildDirErr err) =
  Log.err ("Failed to setup build directory: " <> T.pack (show err))
handleFetchErr (FReadProjectErr err) = handleReadProjectErr err
handleFetchErr (FReadPkgSetErr err) = handleReadPkgSetErr err
handleFetchErr (FFetchPkgErr err) = handleFetchPkgErr err
handleFetchErr (FGraphErr err) = handleGraphErr err

-- | Helper function for handling errors related to
--   readout of package set file.
handleReadPkgSetErr :: MonadLogger m => ReadPkgSetErr -> m ()
handleReadPkgSetErr (PkgSetFileNotFound err) =
  Log.err (T.pack $ "Failed to read package set file: " <> show err <> ".")
handleReadPkgSetErr (PkgSetParseErr err) =
  Log.err (T.pack $ "Failed to parse package set file: " <> err <> ".")

-- | Helper function for handling erros when fetching dependencies.
handleFetchPkgErr :: MonadLogger m => FetchPkgErr -> m ()
handleFetchPkgErr (FPPkgMissingInPkgSet projName) =
  Log.err ("Package missing in idr-package-set.json: " <> unProjName projName <> ".")
handleFetchPkgErr (FPReadPkgDepsErr err) = handleReadPkgErr err
handleFetchPkgErr (FPReadProjectErr err) = handleReadProjectErr err
handleFetchPkgErr (FPGitCloneErr err) = handleGitCloneErr err
handleFetchPkgErr (FPReadDirErr err) =
  Log.err ("Failed to check if directory exists: " <> T.pack (show err))
handleFetchPkgErr (FPReadFileErr err) =
  Log.err ("Failed to check if file exists: " <> T.pack (show err))

-- | Helper function for handling errors when cloning git repositories.
handleGitCloneErr :: MonadLogger m => GitCloneErr -> m ()
handleGitCloneErr (CloneFailError (Repo repo) err) =
  Log.err ("Error occurred during cloning of git repo (" <> repo <> "): " <> T.pack (show err) <> ".")
handleGitCloneErr (CloneFailExitCode (Repo repo) code) =
  Log.err ("Cloning of git repo (" <> repo <> ") returned non-zero exit code: "
          <> T.pack (show code) <> ".")
handleGitCloneErr (CheckoutFailError (Repo repo) (Version vsn) err) =
  Log.err ("Error occurred during checkout of repo (" <> repo
          <> "), version = " <> vsn <> ":" <> T.pack (show err))
handleGitCloneErr (CheckoutFailExitCode (Repo repo) (Version vsn) code) =
  Log.err ("Checkout of repo (" <> repo <> "), version = " <> vsn
          <> " returned non-zero exit code: " <> T.pack (show code))

