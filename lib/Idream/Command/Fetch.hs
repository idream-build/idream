
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Fetch ( fetchDeps ) where


-- Imports

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Logger
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
import Idream.Command.Common ( setupBuildDir, tryAction, readProjFile, readPkgFile
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
data FetchErr = FReadProjectErr ReadProjectErr
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
                 deriving (Eq, Show)


-- Functions

-- TODO add force fetch flag


-- | Top level function that tries to fetch all dependencies.
fetchDeps :: ( MonadReader Config m, MonadLogger m, MonadIO m ) => m ()
fetchDeps = do
  setupBuildDir
  workDir <- asks $ buildDir . buildSettings
  result <- runExceptT $ do
    rootProj@(Project projName _) <- withExceptT FReadProjectErr readRootProjFile
    pkgSet <- withExceptT FReadPkgSetErr readPkgSetFile
    $(logInfo) ("Fetching dependencies for " <> unProjName projName <> ".")
    let initialGraph = mkGraphFromProject rootProj
        graphFile = workDir </> "dependency-graph.json"
        fetchDepsForProj = fetchDepsForProject pkgSet rootProj
    graph <- withExceptT FFetchPkgErr $ execStateT fetchDepsForProj initialGraph
    withExceptT FGraphErr $ saveGraphToJSON graphFile graph
  case result of
    Left err -> handleFetchErr err
    Right _ -> $(logInfo) "Successfully fetched dependencies!"

-- | Recursively fetches all dependencies for a project.
fetchDepsForProject :: ( MonadError FetchPkgErr m
                       , MonadState DepGraph m
                       , MonadReader Config m
                       , MonadLogger m
                       , MonadIO m )
                    => PackageSet -> Project -> m ()
fetchDepsForProject pkgSet (Project projName pkgs) = do
  $(logDebug) ("Fetching dependencies for project: " <> unProjName projName <> ".")
  mapM_ (fetchDepsForPackage pkgSet projName) pkgs

-- | Recursively fetch all dependencies for a package
fetchDepsForPackage :: ( MonadError FetchPkgErr m
                       , MonadState DepGraph m
                       , MonadReader Config m
                       , MonadLogger m
                       , MonadIO m )
                    => PackageSet -> ProjectName -> PackageName -> m ()
fetchDepsForPackage pkgSet projName pkgName = do
  $(logDebug) ("Fetching dependencies for package: " <> unPkgName pkgName <> ".")
  pkgDepsResult <- runExceptT $ readPkgDeps projName pkgName
  case pkgDepsResult of
    Left err -> throwError $ FPReadPkgDepsErr err
    Right pkgDeps -> do
      let isOld (AlreadyFetched _) = True
          isOld _ = False
      (oldSubProjects, newSubProjects) <- partition isOld <$> mapM (fetchProj pkgSet) pkgDeps
      let unwrap (AlreadyFetched proj) = proj
          unwrap (NewlyFetched proj) = proj
          oldSubProjects' = unwrap <$> oldSubProjects
          newSubProjects' = unwrap <$> newSubProjects
          subProjects = oldSubProjects' ++ newSubProjects'
      modify $ updateGraph (DepNode pkgName projName) subProjects
      mapM_ (fetchDepsForProject pkgSet) newSubProjects'

-- | Fetches a package as specified in the top level package set file.
fetchProj :: ( MonadError FetchPkgErr m
             , MonadReader Config m
             , MonadLogger m
             , MonadIO m )
          => PackageSet -> ProjectName -> m FetchInfo
fetchProj (PackageSet pkgs) projName@(ProjectName name) =
  case Map.lookup name pkgs of
    Nothing -> throwError $ FPPkgMissingInPkgSet projName
    Just (PackageDescr repo version) -> do
      workDir <- asks $ buildDir . buildSettings
      projectFilePath <- asks $ projectFile . buildSettings
      let repoDir = workDir </> "src" </> T.unpack name
          projFile = repoDir </> projectFilePath
      (dirExists, fileExists) <- liftIO $ liftM2 (,) (doesDirectoryExist repoDir) (doesFileExist projFile)
      case (dirExists, fileExists) of
        (True, True) -> do
          projInfo <- runExceptT $ readProjFile projFile
          either (throwError . FPReadProjectErr) (return . AlreadyFetched) projInfo
        _ -> do
          cloneResult <- runExceptT $ gitClone repo version repoDir
          either (throwError . FPGitCloneErr) return cloneResult
          result <- runExceptT $ readProjFile projFile
          either (throwError . FPReadProjectErr) (return . NewlyFetched) result

-- | Reads out the top level project file (idr-project.json).
readRootProjFile :: ( MonadError ReadProjectErr m
                    , MonadReader Config m
                    , MonadIO m )
                 => m Project
readRootProjFile = do
  projectFilePath <- asks $ projectFile . buildSettings
  readProjFile projectFilePath

-- | Reads the package file to determine the project dependencies.
readPkgDeps :: ( MonadError ReadPkgErr m
               , MonadReader Config m
               , MonadLogger m
               , MonadIO m )
            => ProjectName -> PackageName -> m [ProjectName]
readPkgDeps (ProjectName projName) (PackageName pkgName) = do
  workDir <- asks $ buildDir . buildSettings
  pkgFileName <- asks $ pkgFile . buildSettings
  let pkgDir = workDir </> "src" </> T.unpack projName </> T.unpack pkgName
      pkgFilePath = pkgDir </> pkgFileName
  (Package _ _ _ deps) <- readPkgFile pkgFilePath
  return deps

-- | Reads out the package set file (idr-package-set.json).
readPkgSetFile :: ( MonadError ReadPkgSetErr m
                  , MonadReader Config m
                  , MonadLogger m
                  , MonadIO m )
               => m PackageSet
readPkgSetFile = do
  pkgSetFilePath <- asks $ pkgSetFile . buildSettings
  pkgSetJSON <- tryAction PkgSetFileNotFound $ do
    dir <- getCurrentDirectory
    BSL.readFile $ dir </> pkgSetFilePath
  let result = eitherDecode pkgSetJSON
  either (throwError . PkgSetParseErr) return result

-- | Clones a git repo, along with all it's submodules into a directory.
gitClone :: ( MonadError GitCloneErr m
            , MonadIO m )
         => Repo -> Version -> Directory -> m ()
gitClone r@(Repo repo) v@(Version vsn) repoDir =
  let cloneArgs = ["clone", "--quiet", "--recurse-submodules", T.unpack repo, repoDir]
      checkoutArgs = ["checkout", "--quiet", T.unpack vsn]
      execProcess command cmdArgs maybeDir = do
        (_, _, _, procHandle) <- createProcess (proc command cmdArgs) { cwd = maybeDir }
        waitForProcess procHandle
  in do
    cloneResult <- tryAction (CloneFailError r) $ execProcess "git" cloneArgs Nothing
    when (cloneResult /= ExitSuccess) $ throwError $ CloneFailExitCode r cloneResult
    checkoutResult <- tryAction (CheckoutFailError r v) $ execProcess "git" checkoutArgs (Just repoDir)
    when (checkoutResult /= ExitSuccess) $ throwError $ CheckoutFailExitCode r v checkoutResult


-- | Helper function for handling errors that can occur during
--   fetching of dependencies.
handleFetchErr :: MonadLogger m => FetchErr -> m ()
handleFetchErr (FReadProjectErr err) = handleReadProjectErr err
handleFetchErr (FReadPkgSetErr err) = handleReadPkgSetErr err
handleFetchErr (FFetchPkgErr err) = handleFetchPkgErr err
handleFetchErr (FGraphErr err) = handleGraphErr err

-- | Helper function for handling errors related to
--   readout of package set file.
handleReadPkgSetErr :: MonadLogger m => ReadPkgSetErr -> m ()
handleReadPkgSetErr (PkgSetFileNotFound err) =
  $(logError) (T.pack $ "Failed to read package set file: " <> show err <> ".")
handleReadPkgSetErr (PkgSetParseErr err) =
  $(logError) (T.pack $ "Failed to parse package set file: " <> err <> ".")

-- | Helper function for handling erros when fetching dependencies.
handleFetchPkgErr :: MonadLogger m => FetchPkgErr -> m ()
handleFetchPkgErr (FPPkgMissingInPkgSet projName) =
  $(logError) ("Package missing in idr-package-set.json: " <> unProjName projName <> ".")
handleFetchPkgErr (FPReadPkgDepsErr err) = handleReadPkgErr err
handleFetchPkgErr (FPReadProjectErr err) = handleReadProjectErr err
handleFetchPkgErr (FPGitCloneErr err) = handleGitCloneErr err

-- | Helper function for handling errors when cloning git repositories.
handleGitCloneErr :: MonadLogger m => GitCloneErr -> m ()
handleGitCloneErr (CloneFailError (Repo repo) err) =
  $(logError) ("Error occurred during cloning of git repo (" <> repo <> "): " <> T.pack (show err) <> ".")
handleGitCloneErr (CloneFailExitCode (Repo repo) code) =
  $(logError) ("Cloning of git repo (" <> repo <> ") returned non-zero exit code: "
               <> T.pack (show code) <> ".")
handleGitCloneErr (CheckoutFailError (Repo repo) (Version vsn) err) =
  $(logError) ("Error occurred during checkout of repo (" <> repo
               <> "), version = " <> vsn <> ":" <> T.pack (show err))
handleGitCloneErr (CheckoutFailExitCode (Repo repo) (Version vsn) code) =
  $(logError) ("Checkout of repo (" <> repo <> "), version = " <> vsn
               <> " returned non-zero exit code: " <> T.pack (show code))

