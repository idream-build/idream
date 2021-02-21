module Idream.Command.Common
  ( setupBuildDir
  , readProjFile
  , readRootProjFile
  , readPkgFile
  , getPkgDirPath
  , getPkgFilePath
  , PkgParseErr(..)
  , ProjParseErr(..)
  ) where

import Control.Exception (Exception)
import qualified Data.Text as T
import Idream.App (AppM, appCreateDir, appReadJSON)
import Idream.FilePaths (Directory, buildDir, pkgFile, projectFile)
import Idream.ToText (ToText (..))
import Idream.Types (Package (..), PackageName (..), Project (..), ProjectName (..))
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

-- | Creates a build directory in which idream will store all build artifacts.
setupBuildDir :: AppM ()
setupBuildDir = appCreateDir buildDir

-- | Reads out a project file (idr-project.json).
readProjFile :: FilePath -> AppM Project
readProjFile = appReadJSON ProjParseErr

-- | Reads out the top level project file (idr-project.json).
readRootProjFile :: AppM Project
readRootProjFile = readProjFile projectFile

-- | Reads out a package file (idr-package.json)
readPkgFile :: FilePath -> AppM Package
readPkgFile = appReadJSON PkgParseErr

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
