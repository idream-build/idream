-- | idream-specific constants and path manipulation functions.
module Idream.FileLogic
  ( workDir
  , buildDirName
  , buildDir
  , outputDirName
  , outputDir
  , fetchDirName
  , fetchDir
  , installDirName
  , installDir
  , pkgBuildDir
  , pkgOutputDir
  , pkgInstallDir
  , repoFetchDir
  , projFileName
  , pkgFileName
  , pkgSetFileName
  ) where

import qualified Data.Text as T
import Idream.Prelude
import Idream.Types.Common (PackageName, RepoName)

-- NOTE: all these functions return a path relative to root of a project!

-- | Work directory that idream uses to store build artifacts + more in.
workDir :: Directory
workDir = ".idream-work"

buildDirName :: Directory
buildDirName = "build"

buildDir :: Directory
buildDir = workDir </> buildDirName

outputDirName :: Directory
outputDirName = "output"

outputDir :: Directory
outputDir = workDir </> outputDirName

fetchDirName :: Directory
fetchDirName = "fetch"

fetchDir :: Directory
fetchDir = workDir </> fetchDirName

installDirName :: Directory
installDirName = "install"

installDir :: Directory
installDir = workDir </> installDirName

-- | Directory that is used for storing build artifacts of a specific package.
pkgBuildDir :: PackageName -> Directory
pkgBuildDir = (buildDir </>) . toString

-- | Directory that is used for storing output artifacts of a specific package.
pkgOutputDir :: PackageName -> Directory
pkgOutputDir = (outputDir </>) . toString

-- | Directory that is used for storing install artifacts of a specific package.
pkgInstallDir :: PackageName -> Directory
pkgInstallDir = (installDir </>) . toString

-- | Directory where a dependency is downloaded to.
repoFetchDir :: RepoName -> Directory
repoFetchDir = (fetchDir </>) . toString

-- | File which contains project information.
projFileName :: FilePath
projFileName = "idr-project.json"

-- | File which contains package information.
pkgFileName :: FilePath
pkgFileName = "idr-package.json"

-- | File which contains package set information.
pkgSetFileName :: FilePath
pkgSetFileName = "idr-package-set.json"
