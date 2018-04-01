
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Fetch ( fetchDeps ) where


-- Imports

import Control.Monad.Reader
import Control.Monad.State
import qualified Idream.Log as Log
import Idream.Log ( MonadLogger )
import Idream.SafeIO
import Control.Exception ( IOException )
import System.Directory ( getCurrentDirectory, doesDirectoryExist, doesFileExist  )
import System.FilePath ( (</>) )
import System.Process ( createProcess, waitForProcess, cwd, proc )
import System.Exit ( ExitCode(..) )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Monoid ( (<>) )
import Data.Aeson ( eitherDecode )
import Data.List ( partition )
import qualified Data.Text as T
import Idream.Command.Common ( setupBuildDir, readProjFile, readPkgFile
                             , handleReadProjectErr, handleReadPkgErr
                             , ReadProjectErr(..), ReadPkgErr(..) )
import Idream.Types
import Idream.Graph


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
fetchDeps :: ( MonadReader Config m, MonadLogger m, MonadIO m ) => m ()
fetchDeps = do
  workDir <- asks $ buildDir . buildSettings
  result <- runSafeIO $ do
    setupBuildDir FSetupBuildDirErr
    rootProj@(Project projName _) <- readRootProjFile FReadProjectErr
    pkgSet <- readPkgSetFile FReadPkgSetErr
    Log.info ("Fetching dependencies for " <> unProjName projName <> ".")
    let initialGraph = mkGraphFromProject rootProj
        graphFile = workDir </> "dependency-graph.json"
        fetchDepsForProj = fetchDepsForProject FFetchPkgErr pkgSet rootProj
    graph <- execStateT fetchDepsForProj initialGraph
    saveGraphToJSON FGraphErr graphFile graph
  case result of
    Left err -> handleFetchErr err
    Right _ -> Log.info "Successfully fetched dependencies!"

-- | Recursively fetches all dependencies for a project.
fetchDepsForProject :: ( MonadState DepGraph m
                       , MonadReader Config m
                       , MonadLogger m
                       , MonadSafeIO e m )
                    => (FetchPkgErr -> e) -> PackageSet -> Project -> m ()
fetchDepsForProject f pkgSet (Project projName pkgs) = do
  Log.debug ("Fetching dependencies for project: " <> unProjName projName <> ".")
  mapM_ (fetchDepsForPackage f pkgSet projName) pkgs

-- | Recursively fetch all dependencies for a package
fetchDepsForPackage :: ( MonadState DepGraph m
                       , MonadReader Config m
                       , MonadLogger m
                       , MonadSafeIO e m )
                    => (FetchPkgErr -> e) -> PackageSet -> ProjectName -> PackageName -> m ()
fetchDepsForPackage f pkgSet projName pkgName = do
  Log.debug ("Fetching dependencies for package: " <> unPkgName pkgName <> ".")
  pkgDeps <- readPkgDeps (f . FPReadPkgDepsErr) projName pkgName
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

-- | Fetches a package as specified in the top level package set file.
fetchProj :: ( MonadReader Config m
             , MonadLogger m
             , MonadSafeIO e m )
          => (FetchPkgErr -> e) -> PackageSet -> ProjectName -> m FetchInfo
fetchProj f (PackageSet pkgs) projName@(ProjectName name) =
  case Map.lookup name pkgs of
    Nothing -> raiseError . f $ FPPkgMissingInPkgSet projName
    Just (PackageDescr repo version) -> do
      workDir <- asks $ buildDir . buildSettings
      projectFilePath <- asks $ projectFile . buildSettings
      let repoDir = workDir </> "src" </> T.unpack name
          projFile = repoDir </> projectFilePath
      (dirExists, fileExists) <- liftM2 (,) (checkDirExists f repoDir) (checkFileExists f projFile)
      case (dirExists, fileExists) of
        (True, True) ->
          AlreadyFetched <$> readProjFile (f . FPReadProjectErr) projFile
        _ -> do
          gitClone (f . FPGitCloneErr) repo version repoDir
          result <- readProjFile (f . FPReadProjectErr) projFile
          return $ NewlyFetched result

-- | Reads out the top level project file (idr-project.json).
readRootProjFile :: ( MonadReader Config m, MonadSafeIO e m )
                 => (ReadProjectErr -> e) -> m Project
readRootProjFile f = do
  projectFilePath <- asks $ projectFile . buildSettings
  readProjFile f projectFilePath

-- | Reads the package file to determine the project dependencies.
readPkgDeps :: ( MonadReader Config m, MonadLogger m, MonadSafeIO e m )
            => (ReadPkgErr -> e) -> ProjectName -> PackageName -> m [ProjectName]
readPkgDeps f (ProjectName projName) (PackageName pkgName) = do
  workDir <- asks $ buildDir . buildSettings
  pkgFileName <- asks $ pkgFile . buildSettings
  let pkgDir = workDir </> "src" </> T.unpack projName </> T.unpack pkgName
      pkgFilePath = pkgDir </> pkgFileName
  (Package _ _ _ deps) <- readPkgFile f pkgFilePath
  return deps

-- | Reads out the package set file (idr-package-set.json).
readPkgSetFile :: ( MonadReader Config m, MonadLogger m, MonadSafeIO e m )
               => (ReadPkgSetErr -> e) -> m PackageSet
readPkgSetFile f = do
  pkgSetFilePath <- asks $ pkgSetFile . buildSettings
  pkgSetJSON <- liftSafeIO (f . PkgSetFileNotFound) $ do
    dir <- getCurrentDirectory
    BSL.readFile $ dir </> pkgSetFilePath
  let result = eitherDecode pkgSetJSON
  either (raiseError . f . PkgSetParseErr) return result

-- | Clones a git repo, along with all it's submodules into a directory.
gitClone :: MonadSafeIO e m
         => (GitCloneErr -> e) -> Repo -> Version -> Directory -> m ()
gitClone f r@(Repo repo) v@(Version vsn) repoDir =
  let cloneArgs = ["clone", "--quiet", "--recurse-submodules", T.unpack repo, repoDir]
      checkoutArgs = ["checkout", "--quiet", T.unpack vsn]
      execProcess f' command cmdArgs maybeDir = liftSafeIO f' $ do
        (_, _, _, procHandle) <- createProcess (proc command cmdArgs) { cwd = maybeDir }
        waitForProcess procHandle
  in do
    cloneResult <- execProcess (f . CloneFailError r) "git" cloneArgs Nothing
    when (cloneResult /= ExitSuccess) $ raiseError . f $ CloneFailExitCode r cloneResult
    checkoutResult <- execProcess (f . CheckoutFailError r v) "git" checkoutArgs (Just repoDir)
    when (checkoutResult /= ExitSuccess) $ raiseError . f $ CheckoutFailExitCode r v checkoutResult

-- | Helper function for checking if a directory exists.
checkDirExists :: MonadSafeIO e m => (FetchPkgErr -> e) -> Directory -> m Bool
checkDirExists f dir = liftSafeIO (f . FPReadDirErr) $ doesDirectoryExist dir

-- | Helper function for checking if a file exists.
checkFileExists :: MonadSafeIO e m => (FetchPkgErr -> e) -> FilePath -> m Bool
checkFileExists f path = liftSafeIO (f . FPReadFileErr) $ doesFileExist path

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

