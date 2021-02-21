module Idream.Command.Common
  ( setupBuildDir
  , readProjFile
  , readRootProjFile
  , readPkgFile
  , readPkgSetFile
  , readRootPkgSetFile
  , getPkgDirPath
  , getPkgFilePath
  , PkgParseErr (..)
  , PkgSetParseErr (..)
  , ProjParseErr (..)
  , RootProjMissingErr (..)
  , RootPkgSetMissingErr (..)
  ) where

import Control.Exception (Exception (..))
import qualified Data.Text as T
import Idream.App (AppM)
import Idream.Effects.FileSystem (fsCreateDir, fsDoesFileExist)
import Idream.Effects.Serde (serdeReadJSON)
import Idream.FileLogic (buildDir, pkgFile, pkgSetFile, projectFile)
import Idream.FilePaths (Directory)
import Idream.Types (Package (..), PackageName (..), PackageSet, Project (..), ProjectName (..))
import System.FilePath ((</>))
import UnliftIO.Exception (throwIO)

-- | Error type for describing errors when parsing project file.
newtype ProjParseErr = ProjParseErr String
  deriving (Eq, Show)

instance Exception ProjParseErr where
  displayException (ProjParseErr err) =
    "Failed to parse project file: " <> err <> "."

-- | Error type used for describing errors that can occur while reading out a package file.
newtype PkgParseErr = PkgParseErr String
  deriving (Eq, Show)

instance Exception PkgParseErr where
  displayException (PkgParseErr err) =
    "Failed to parse package file: " <> err <> "."

-- | Error type used for describing errors that can occur while reading out a package set file.
newtype PkgSetParseErr = PkgSetParseErr String
  deriving (Eq, Show)

instance Exception PkgSetParseErr where
  displayException (PkgSetParseErr err) =
    "Failed to parse package set file: " <> err <> "."

-- | Error type indicating that the root project is missing.
data RootProjMissingErr = RootProjMissingErr
  deriving (Eq, Show)

instance Exception RootProjMissingErr where
  displayException RootProjMissingErr =
    "Missing root project at " <> projectFile <> "."

-- | Error type indicating that the root package set is missing.
data RootPkgSetMissingErr = RootPkgSetMissingErr
  deriving (Eq, Show)

instance Exception RootPkgSetMissingErr where
  displayException RootPkgSetMissingErr =
    "Missing root package set at " <> pkgSetFile <> "."

-- | Creates a build directory in which idream will store all build artifacts.
setupBuildDir :: AppM ()
setupBuildDir = fsCreateDir buildDir

-- | Reads out a project file (idr-project.json).
readProjFile :: FilePath -> AppM Project
readProjFile = serdeReadJSON ProjParseErr

-- | Reads out the top level project file (idr-project.json).
readRootProjFile :: AppM Project
readRootProjFile = do
  exists <- fsDoesFileExist projectFile
  if exists
    then readProjFile projectFile
    else throwIO RootProjMissingErr

-- | Reads out a package file (idr-package.json)
readPkgFile :: FilePath -> AppM Package
readPkgFile = serdeReadJSON PkgParseErr

-- | Reads out a package set file (idr-package-set.json)
readPkgSetFile :: FilePath -> AppM PackageSet
readPkgSetFile = serdeReadJSON PkgSetParseErr

-- | Reads out the top level package set file (idr-package-set.json).
readRootPkgSetFile :: AppM PackageSet
readRootPkgSetFile = do
  exists <- fsDoesFileExist pkgSetFile
  if exists
    then readPkgSetFile pkgSetFile
    else throwIO RootPkgSetMissingErr

-- Helper function to determine location of package directory.
getPkgDirPath :: PackageName -> ProjectName -> AppM Directory
getPkgDirPath pkg@(PackageName pkgName) (ProjectName projName) = do
  (Project _ rootPkgNames) <- readRootProjFile
  let basePath = if pkg `elem` rootPkgNames
                   then "."
                   else buildDir </> "src" </> T.unpack projName
  pure (basePath </> T.unpack pkgName)

-- Helper function to determine location of package file.
getPkgFilePath :: PackageName -> ProjectName -> AppM FilePath
getPkgFilePath pkgName projName = (</> pkgFile) <$> getPkgDirPath pkgName projName
