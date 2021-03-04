-- | idream-specific constants and path manipulation functions.
module Idream.FileLogic
  ( workDir
  , buildDir
  , outputDir
  , fetchDir
  , installDir
  , pkgBuildDir
  , pkgOutputDir
  , repoDir
  , projFileName
  , pkgFileName
  , ipkgFileName
  , pkgSetFileName
  , depGraphMapFile
  , depInfoMapFile
  ) where

import qualified Data.Text as T
import Idream.Prelude
import Idream.Types.Common (PackageName (..), RepoName (..))

-- NOTE: all these functions return a path relative to root of a project!

-- | Work directory that idream uses to store build artifacts + more in.
workDir :: Directory
workDir = ".idream-work"

buildDir :: Directory
buildDir = workDir </> "build"

outputDir :: Directory
outputDir = workDir </> "output"

fetchDir :: Directory
fetchDir = workDir </> "fetch"

installDir :: Directory
installDir = workDir </> "install"

-- | Directory that is used for storing build artifacts of a specific package.
pkgBuildDir :: PackageName -> Directory
pkgBuildDir (PackageName pkgName) =
  buildDir </> T.unpack pkgName

-- | Directory that is used for storing output artifacts of a specific package.
pkgOutputDir :: PackageName -> Directory
pkgOutputDir (PackageName pkgName) =
  outputDir </> T.unpack pkgName

-- | Directory where a dependency is downloaded to.
repoDir :: RepoName -> Directory
repoDir (RepoName repoName) =
  fetchDir </> T.unpack repoName

-- | File which contains project information.
projFileName :: FilePath
projFileName = "idr-project.json"

-- | File which contains package information.
pkgFileName :: FilePath
pkgFileName = "idr-package.json"

-- | Ipkg file
ipkgFileName :: PackageName -> FilePath
ipkgFileName (PackageName pkgName) = T.unpack (pkgName <> ".ipkg")

-- | File which contains package set information.
pkgSetFileName :: FilePath
pkgSetFileName = "idr-package-set.json"

-- | Location of the dependency graph file.
depGraphMapFile :: FilePath
depGraphMapFile = workDir </> "dependency-graph.json"

-- | Location of the dependency info file.
depInfoMapFile :: FilePath
depInfoMapFile = workDir </> "dependency-info.json"
