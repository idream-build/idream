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

import Control.Exception (Exception)
import qualified Data.Text as T
import Idream.App (AppM, appCreateDir, appDoesFileExist, appLogAndThrow, appReadJSON)
import Idream.FilePaths (Directory, buildDir, pkgFile, pkgSetFile, projectFile)
import Idream.ToText (ToText (..))
import Idream.Types (Package (..), PackageName (..), PackageSet, Project (..), ProjectName (..))
import System.FilePath ((</>))

-- | Error type for describing errors when parsing project file.
newtype ProjParseErr = ProjParseErr String
  deriving (Eq, Show)

instance Exception ProjParseErr

instance ToText ProjParseErr where
  toText (ProjParseErr err) =
    "Failed to parse project file: " <> toText err <> "."

-- | Error type used for describing errors that can occur while reading out a package file.
newtype PkgParseErr = PkgParseErr String
  deriving (Eq, Show)

instance Exception PkgParseErr

instance ToText PkgParseErr where
  toText (PkgParseErr err) =
    "Failed to parse package file: " <> toText err <> "."

-- | Error type used for describing errors that can occur while reading out a package set file.
newtype PkgSetParseErr = PkgSetParseErr String
  deriving (Eq, Show)

instance Exception PkgSetParseErr

instance ToText PkgSetParseErr where
  toText (PkgSetParseErr err) =
    "Failed to parse package set file: " <> toText err <> "."

-- | Error type indicating that the root project is missing.
data RootProjMissingErr = RootProjMissingErr
  deriving (Eq, Show)

instance Exception RootProjMissingErr

instance ToText RootProjMissingErr where
  toText RootProjMissingErr =
    "Missing root project at " <> toText projectFile <> "."

-- | Error type indicating that the root package set is missing.
data RootPkgSetMissingErr = RootPkgSetMissingErr
  deriving (Eq, Show)

instance Exception RootPkgSetMissingErr

instance ToText RootPkgSetMissingErr where
  toText RootPkgSetMissingErr =
    "Missing root package set at " <> toText pkgSetFile <> "."

-- | Creates a build directory in which idream will store all build artifacts.
setupBuildDir :: AppM ()
setupBuildDir = appCreateDir buildDir

-- | Reads out a project file (idr-project.json).
readProjFile :: FilePath -> AppM Project
readProjFile = appReadJSON ProjParseErr

-- | Reads out the top level project file (idr-project.json).
readRootProjFile :: AppM Project
readRootProjFile = do
  exists <- appDoesFileExist projectFile
  if exists
    then readProjFile projectFile
    else appLogAndThrow RootProjMissingErr

-- | Reads out a package file (idr-package.json)
readPkgFile :: FilePath -> AppM Package
readPkgFile = appReadJSON PkgParseErr

-- | Reads out a package set file (idr-package-set.json)
readPkgSetFile :: FilePath -> AppM PackageSet
readPkgSetFile = appReadJSON PkgSetParseErr

-- | Reads out the top level package set file (idr-package-set.json).
readRootPkgSetFile :: AppM PackageSet
readRootPkgSetFile = do
  exists <- appDoesFileExist pkgSetFile
  if exists
    then readPkgSetFile pkgSetFile
    else appLogAndThrow RootPkgSetMissingErr

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
