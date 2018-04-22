
module Idream.Command.Fetch ( fetchDeps ) where


-- Imports

import Prelude hiding ( readFile )
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Error
import Control.Monad.Reader
import qualified Idream.Effects.Log as Log
import Idream.Effects.Log ( Logger, logErr )
import Idream.SafeIO
import Idream.Command.Common ( ProjParseErr(..), PkgParseErr(..)
                             , setupBuildDir, readRootProjFile, readProjFile
                             , readPkgFile, getPkgFilePath
                             , handleReadProjectErr, handleReadPkgErr )
import Idream.Types
import Idream.Graph
import Idream.Effects.FileSystem
import Idream.Effects.Git
import qualified Data.Map as Map
import Data.Monoid ( (<>) )
import Data.Aeson ( eitherDecode )
import Data.List ( partition )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding ( encodeUtf8 )


-- Types

-- | Helper data type used for marking if a project was already fetched or not.
--   This helps detecting/breaking a cycle in the dependency graph.
data FetchInfo = AlreadyFetched Project
               | NewlyFetched Project
               deriving (Eq, Show)

-- | Top level error type, models all errors that can occur
--   during fetching of dependencies.
data FetchErr = FFSErr FSError
              | FLogErr Log.LogError
              | FGitErr GitError
              | FRootProjParseErr ProjParseErr
              | FProjParseErr ProjectName ProjParseErr
              | FPkgParseErr ProjectName PackageName PkgParseErr
              | FPkgSetParseErr String
              | FPkgMissingInPkgSet ProjectName
              | FGraphErr ParseGraphErr
              deriving (Eq, Show)


-- Functions

-- TODO add force fetch flag


runProgram :: ( MonadReader Config m, MonadIO m )
           => Eff '[Logger, Error FetchErr, Git, FileSystem, SafeIO FetchErr] ()
           -> m (Either FetchErr ())
runProgram prog = do
  thres <- asks $ logLevel . args
  liftIO $  fmap join
         $  runSafeIO
        <$> runM
         .  runFS FFSErr
         .  runGit FGitErr
         .  runError
         .  Log.runLogger FLogErr thres
         $  prog

-- | Top level function that tries to fetch all dependencies.
fetchDeps :: ( MonadReader Config m, MonadIO m ) => m ()
fetchDeps = do
  result <- runProgram $ do
    projFileExists <- doesFileExist projectFile
    if not projFileExists
      then Log.err "Did not find project file, aborting."
      else do
        fetchDeps'
        Log.info "Finished fetching dependencies."
  either (logErr . handleFetchErr) return result

-- | Helper function that does the actual fetching of dependencies.
fetchDeps' :: ( Member Logger r
              , Member (Error FetchErr) r
              , Member Git r
              , Member FileSystem r )
           => Eff r ()
fetchDeps' = do
  setupBuildDir
  rootProj@(Project projName rootPkgs) <- readRootProjFile FRootProjParseErr
  if null rootPkgs
    then Log.info ("Project contains no packages yet, skipping fetch step. "
                <> "Use `idream add` to add a package to this project first.")
    else do
      pkgSet <- readPkgSetFile
      Log.info ("Fetching dependencies for " <> unProjName projName <> ".")
      let initialGraph = mkGraphFromProject rootProj
          fetchDepsForProj = fetchDepsForProject pkgSet rootProj
      graph <- execState initialGraph fetchDepsForProj
      saveGraphToJSON depGraphFile graph


-- | Recursively fetches all dependencies for a project.
fetchDepsForProject :: ( Member (State DepGraph) r
                       , Member (Error FetchErr) r
                       , Member FileSystem r
                       , Member Git r
                       , Member Logger r )
                    => PackageSet -> Project -> Eff r ()
fetchDepsForProject pkgSet (Project projName pkgs) = do
  Log.debug ("Fetching dependencies for project: " <> unProjName projName <> ".")
  mapM_ (fetchDepsForPackage pkgSet projName) pkgs

-- | Recursively fetch all dependencies for a package
fetchDepsForPackage :: ( Member (Error FetchErr) r
                       , Member (State DepGraph) r
                       , Member Git r
                       , Member Logger r
                       , Member FileSystem r )
                    => PackageSet -> ProjectName -> PackageName -> Eff r ()
fetchDepsForPackage pkgSet projName pkgName = do
  Log.debug ("Fetching dependencies for package: " <> unPkgName pkgName <> ".")
  pkgDeps <- readPkgDeps projName pkgName
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

-- | Fetches a project as specified in the top level package set file.
fetchProj :: ( Member Logger r, Member (Error FetchErr) r
             , Member Git r, Member FileSystem r )
          => PackageSet -> ProjectName -> Eff r FetchInfo
fetchProj (PackageSet pkgs) projName@(ProjectName name) =
  case Map.lookup name pkgs of
    Nothing -> throwError  $ FPkgMissingInPkgSet projName
    Just (PackageDescr repo version) -> do
      let repoDir' = repoDir projName
          projFile = repoDirProjFile projName
      (dirExists, fileExists) <- liftM2 (,) (doesDirExist repoDir') (doesFileExist projFile)
      case (dirExists, fileExists) of
        (True, True) ->
          AlreadyFetched <$> readProjFile (FProjParseErr projName) projFile
        _ -> do
          gitClone repo repoDir'
          gitCheckout repo version repoDir'
          result <- readProjFile (FProjParseErr projName) projFile
          return $ NewlyFetched result

-- | Reads the package file to determine the project dependencies.
readPkgDeps :: ( Member FileSystem r, Member (Error FetchErr) r, Member Logger r )
            => ProjectName -> PackageName -> Eff r [ProjectName]
readPkgDeps projName pkgName = do
  pkgFilePath <- getPkgFilePath (FProjParseErr projName) pkgName projName
  (Package _ _ _ deps) <- readPkgFile (FPkgParseErr projName pkgName) pkgFilePath
  return deps

-- | Reads out the package set file (idr-package-set.json).
readPkgSetFile :: ( Member FileSystem r
                  , Member (Error FetchErr) r )
               => Eff r PackageSet
readPkgSetFile = do
  pkgSetJSON <- encodeUtf8 . TL.fromStrict <$> readFile pkgSetFile
  let result = eitherDecode pkgSetJSON
  either (throwError . FPkgSetParseErr) return result

-- | Helper function for handling errors that can occur during
--   fetching of dependencies.
handleFetchErr :: FetchErr -> T.Text
handleFetchErr (FFSErr err) = handleFSErr err
handleFetchErr (FLogErr err) = Log.handleLogErr err
handleFetchErr (FGitErr err) = handleGitErr err
handleFetchErr (FRootProjParseErr err) =
  "Failed to parse root project file: " <> handleReadProjectErr err <> "."
handleFetchErr (FProjParseErr projName err) =
  "Failed to parse project file (project = "
    <> unProjName projName <> "), reason: "
    <> T.pack (show err) <> "."
handleFetchErr (FPkgParseErr projName pkgName err) =
  "Failed to parse package set file for project: " <> unProjName projName
    <> ", package = " <> unPkgName pkgName <> ": "
    <> handleReadPkgErr err <> "."
handleFetchErr (FPkgSetParseErr err) =
  "Failed to parse package set, reason: " <> T.pack (show err) <> "."
handleFetchErr (FPkgMissingInPkgSet projName) =
  "Package missing in idr-package-set.json: " <> unProjName projName <> "."
handleFetchErr (FGraphErr err) = handleGraphErr err

