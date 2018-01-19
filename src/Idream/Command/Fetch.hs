
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Fetch ( fetchDeps ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Logger
import System.Directory ( getCurrentDirectory
                        , createDirectoryIfMissing )
import System.FilePath ( FilePath, (</>) )
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))
import Data.Either ( partitionEithers )
import Data.Aeson ( eitherDecode )
import Data.Aeson.Text ( encodeToLazyText )
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text as T
import Idream.Types ( Config(..)
                    , Project(..)
                    , PackageSet(..)
                    , PackageName(..)
                    , buildSettings
                    , buildDir
                    , projectFile
                    , pkgSetFile )
type Directory = FilePath


-- TODO error type? better error handling -> MonadError?
type Graph = ()  -- TODO proper type



fetchDeps :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
fetchDeps = do
  setupBuildDir
  rootProjectData <- readRootProjectFile
  case rootProjectData of
    Left err -> liftIO $ putStrLn $ "Failed to get project information: " <> err <> "."
    Right rootProj@(Project pkgName _) -> do
      $(logInfo) ("Fetching dependencies for " <> unName pkgName <> ".")
      let initialGraph = mkGraphFromProject rootProj
          graphFile = "dependency-graph.json"
      completeGraph <- execStateT (fetchDepsForProject rootProj) initialGraph
      saveGraphToJSON completeGraph graphFile
      $(logInfo) "Successfully fetched dependencies!"

-- | Creates a build directory in which idream will store all build artifacts.
setupBuildDir :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
setupBuildDir = do
  $(logDebug) "Setting up build directory."
  buildDirectory <- asks $ buildDir . buildSettings
  liftIO $ createDirectoryIfMissing True buildDirectory

readRootProjectFile :: (MonadReader Config m, MonadIO m)
                    => m (Either String Project)
readRootProjectFile = do
  projectFilePath <- asks $ projectFile . buildSettings
  liftIO $ do
    cwd <- getCurrentDirectory
    projectJSON <- BSL.readFile $ cwd </> projectFilePath
    return $ eitherDecode projectJSON

fetchDepsForProject :: (MonadReader Config m,
                        MonadState Graph m,
                        MonadLogger m,
                        MonadIO m)
                    => Project -> m ()
fetchDepsForProject (Project pkgName deps) = do
  $(logDebug) ("Fetching deps for " <> (unName pkgName))
  -- TODO refactor with MonadError
  -- TODO move pkgset etc out to config...?
  pkgSet <- readPkgSetFile
  case pkgSet of
    Left err -> liftIO . putStrLn $ "Failed to read package set file" <> err
    Right pkgSet' -> do
      projectResults <- mapM (fetchDep pkgSet') deps
      case partitionEithers projectResults of
        ([], projects) -> do
          -- TODO filter already fetched
          modify $ \graph -> updateGraph projects graph
          mapM_ fetchDepsForProject projects
        (errors, _) -> liftIO $ mapM putStrLn errors >> return ()

readPkgSetFile :: (MonadReader Config m, MonadLogger m, MonadIO m)
               => m (Either String PackageSet)
readPkgSetFile = do
  pkgSetFilePath <- asks $ pkgSetFile . buildSettings
  liftIO $ do
    cwd <- getCurrentDirectory
    pkgSetJSON <- BSL.readFile $ cwd </> pkgSetFilePath
    return $ eitherDecode pkgSetJSON

fetchDep :: (MonadReader Config m, MonadLogger m, MonadIO m)
         => PackageSet -> PackageName -> m (Either String Project)
fetchDep (PackageSet pkgs) (PackageName pkgName) = do
  -- TODO find in pkg set file, might be missing...

  return ()


mkGraphFromProject :: Project -> Graph
mkGraphFromProject _ = ()

updateGraph :: [Project] -> Graph -> Graph
updateGraph _ _ = _

saveGraphToJSON :: (MonadIO m) => Graph -> FilePath -> m ()
saveGraphToJSON graph file = liftIO $ TIO.writeFile file (encodeToLazyText graph)



{- TODO:

fetch all deps listed in json file of project => store in .idream-work/src/LIB_NAME
start building graph along the way, to check for cycles/or already fetched deps
repeat until its fully fetched

problems: version differences?
exceptions handling..

-}
