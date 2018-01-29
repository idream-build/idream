
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Fetch ( fetchDeps ) where


-- Imports

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Logger
import Control.Exception ( IOException )
import System.Directory ( getCurrentDirectory )
import System.FilePath ( FilePath, (</>) )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Monoid ( (<>) )
import Data.Aeson ( eitherDecode )
import qualified Data.Text as T
import System.Process ( createProcess, waitForProcess, cwd, proc )
import System.Exit ( ExitCode(..) )
import Idream.Command.Common ( setupBuildDir, tryAction )
import Idream.Types ( Config(..), Project(..), PackageSet(..), PackageName(..)
                    , PackageDescr(..), Repo(..), Version(..), Directory
                    , buildSettings, buildDir, projectFile, pkgSetFile )
import Idream.Graph


-- Types

-- | State used to keep track of already fetched dependencies.
--   Note: during retrieval of dependencies, a dependency graph
--         is also built along the way.
data FetchState = FetchState { depGraph :: DepGraph
                             , fetchedPkgs :: [PackageName]
                             } deriving (Eq, Show)

-- | Top level error type, models all errors that can occur
--   during fetching of dependencies.
data FetchErr = FReadProjectErr ReadProjectErr
              | FReadPkgSetErr ReadPkgSetErr
              | FFetchPkgErr FetchPkgErr
              | FGraphErr GraphErr
              deriving (Eq, Show)

-- | Error type used for describing errors that can occur while reading out the project file.
data ReadProjectErr = ProjectFileNotFound IOException
                    | ProjectParseErr String
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
data FetchPkgErr = FPPkgMissingInPkgSet PackageName
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
    rootProj@(Project pkgName _) <- withExceptT FReadProjectErr readRootProjFile
    pkgSet <- withExceptT FReadPkgSetErr readPkgSetFile
    $(logInfo) ("Fetching dependencies for " <> unName pkgName <> ".")
    let initialGraph = updateGraph [rootProj] emptyGraph
        initialState = FetchState initialGraph [pkgName]
        graphFile = workDir </> "dependency-graph.json"
        fetchDepsForProj = fetchDepsForProject pkgSet rootProj
    graph <- withExceptT FFetchPkgErr $ depGraph <$> execStateT fetchDepsForProj initialState
    withExceptT FGraphErr $ saveGraphToJSON graphFile graph
  case result of
    Left err -> handleFetchErr err
    Right _ -> $(logInfo) "Successfully fetched dependencies!"

-- | Recursively fetches all dependencies for a project.
--   This function avoids potential cycles in dependencies by marking already fetched packages.
fetchDepsForProject :: ( MonadError FetchPkgErr m
                       , MonadState FetchState m
                       , MonadReader Config m
                       , MonadLogger m
                       , MonadIO m )
                    => PackageSet -> Project -> m ()
fetchDepsForProject pkgSet (Project pkgName deps) = do
  $(logDebug) ("Fetching dependencies for package: " <> unName pkgName <> ".")
  alreadyFetchedPkgs <- gets fetchedPkgs
  let notYetFetchedPkgs = filter (`notElem` alreadyFetchedPkgs) deps
  subProjects <- mapM (fetchPkg pkgSet) notYetFetchedPkgs
  modify $ \s -> s { depGraph = updateGraph subProjects (depGraph s)
                   , fetchedPkgs = (fmap projPkgName subProjects) ++ (fetchedPkgs s) }
  mapM_ (fetchDepsForProject pkgSet) subProjects

-- | Fetches a package as specified in the top level package set file.
fetchPkg :: ( MonadError FetchPkgErr m
            , MonadReader Config m
            , MonadLogger m
            , MonadIO m )
         => PackageSet -> PackageName -> m Project
fetchPkg (PackageSet pkgs) pkgName@(PackageName name) = do
  case Map.lookup name pkgs of
    Nothing -> throwError $ FPPkgMissingInPkgSet pkgName
    Just (PackageDescr repo version) -> do
      workDir <- asks $ buildDir . buildSettings
      projectFilePath <- asks $ projectFile . buildSettings
      let repoDir = workDir </> "src" </> (T.unpack name)
          projFile = repoDir </> projectFilePath
      cloneResult <- runExceptT $ gitClone repo version repoDir
      either (throwError . FPGitCloneErr) return cloneResult
      result <- runExceptT $ readProjFile projFile
      either (throwError . FPReadProjectErr) return result

-- | Reads out the top level project file (idr-project.json).
readRootProjFile :: ( MonadError ReadProjectErr m
                    , MonadReader Config m
                    , MonadIO m )
                 => m Project
readRootProjFile = do
  projectFilePath <- asks $ projectFile . buildSettings
  readProjFile projectFilePath

-- | Reads out a project file (idr-project.json).
readProjFile :: ( MonadError ReadProjectErr m
                , MonadIO m )
             => FilePath -> m Project
readProjFile file = do
  projectJSON <- tryAction ProjectFileNotFound $ do
    dir <- getCurrentDirectory
    BSL.readFile $ dir </> file
  either (throwError . ProjectParseErr) return $ eitherDecode projectJSON

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
      execProcess cmd cmdArgs maybeDir = do
        (_, _, _, procHandle) <- createProcess (proc cmd cmdArgs) { cwd = maybeDir }
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
--   readout of project file.
handleReadProjectErr :: MonadLogger m => ReadProjectErr -> m ()
handleReadProjectErr (ProjectFileNotFound err) =
  $(logError) (T.pack $ "Did not find project file: " <> show err <> ".")
handleReadProjectErr (ProjectParseErr err) =
  $(logError) (T.pack $ "Failed to parse project file: " <> err <> ".")

-- | Helper function for handling errors related to
--   readout of package set file.
handleReadPkgSetErr :: MonadLogger m => ReadPkgSetErr -> m ()
handleReadPkgSetErr (PkgSetFileNotFound err) =
  $(logError) (T.pack $ "Failed to read package set file" <> show err <> ".")
handleReadPkgSetErr (PkgSetParseErr err) =
  $(logError) (T.pack $ "Failed to parse package set file" <> err <> ".")

-- | Helper function for handling erros when fetching dependencies.
handleFetchPkgErr :: MonadLogger m => FetchPkgErr -> m ()
handleFetchPkgErr (FPPkgMissingInPkgSet pkgName) =
  $(logError) ("Package missing in idr-package-set.json: " <> unName pkgName <> ".")
handleFetchPkgErr (FPReadProjectErr err) = handleReadProjectErr err
handleFetchPkgErr (FPGitCloneErr err) = handleGitCloneErr err

-- | Helper function for handling errors when cloning git repositories.
handleGitCloneErr :: MonadLogger m => GitCloneErr -> m ()
handleGitCloneErr (CloneFailError (Repo repo) err) =
  $(logError) ("Error occurred during cloning of git repo (" <> repo <> "): " <> (T.pack $ show err) <> ".")
handleGitCloneErr (CloneFailExitCode (Repo repo) code) =
  $(logError) ("Cloning of git repo (" <> repo <> ") returned non-zero exit code: "
               <> (T.pack $ show code) <> ".")
handleGitCloneErr (CheckoutFailError (Repo repo) (Version vsn) err) =
  $(logError) ("Error occurred during checkout of repo (" <> repo
               <> "), version = " <> vsn <> ":" <> (T.pack $ show err))
handleGitCloneErr (CheckoutFailExitCode (Repo repo) (Version vsn) code) =
  $(logError) ("Checkout of repo (" <> repo <> "), version = " <> vsn
               <> " returned non-zero exit code: " <> (T.pack $ show code))

-- | Helper function for handling errors related to saving
--   of the dependency graph to a file.
handleGraphErr :: MonadLogger m => GraphErr -> m ()
handleGraphErr (LoadGraphErr err) =
  $(logError) (T.pack $ "Failed to load graph from a file: " <> show err <> ".")
handleGraphErr (SaveGraphErr err) =
  $(logError) (T.pack $ "Failed to save graph to a file: " <> show err <> ".")
handleGraphErr (ParseGraphErr err) =
  $(logError) (T.pack $ "Failed to parse graph from file: " <> show err <> ".")
