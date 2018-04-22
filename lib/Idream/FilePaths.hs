
module Idream.FilePaths ( Directory, FilePath
                        , pkgDir, pkgSrcDir
                        , buildDir, projectBuildDir
                        , pkgBuildDir, pkgBuildSrcDir
                        , pkgCompileDir, pkgDocsDir
                        , repoDir, repoDirProjFile
                        , projectFile, pkgFile, pkgSetFile
                        , ipkgFile, depGraphFile
                        ) where

-- Imports

import System.FilePath ( FilePath, (</>) )
import Idream.Types ( ProjectName(..), PackageName(..), SourceDir(..) )
import qualified Data.Text as T
import Data.Monoid ( (<>) )


-- Data types

-- | Type alias for directories.
type Directory = FilePath


-- Functions

-- NOTE: all these functions return a path relative to root of a project!

-- | Returns the directory a package in a project is located in.
pkgDir :: PackageName -> Directory
pkgDir (PackageName pkgName) = T.unpack pkgName

-- | Returns the default directory where the idris files of a package in a project are located.
pkgSrcDir :: PackageName -> Directory
pkgSrcDir pkgName = pkgDir pkgName </> "src"

-- | Build directory that idream uses to store build artifacts in.
buildDir :: Directory
buildDir = ".idream-work"

-- | Directory that is used for storing build artifacts of a specific project.
projectBuildDir :: ProjectName -> Directory
projectBuildDir (ProjectName projName) =
  buildDir </> "build" </> T.unpack projName

-- | Directory that is used for storing build artifacts of a specific package in a project.
pkgBuildDir :: ProjectName -> PackageName -> Directory
pkgBuildDir projName (PackageName pkgName) =
  projectBuildDir projName </> T.unpack pkgName

-- | Directory which contains the idris files of a package in the build directory.
pkgBuildSrcDir :: ProjectName -> PackageName -> SourceDir -> Directory
pkgBuildSrcDir projName pkgName (SourceDir dir) =
  pkgBuildDir projName pkgName </> dir

-- | Directory in which compiled files are stored for a package.
pkgCompileDir :: ProjectName -> PackageName -> Directory
pkgCompileDir projName pkgName = pkgBuildDir projName pkgName </> "bin"

-- | Directory in which the docs for a package are stored.
pkgDocsDir :: ProjectName -> PackageName -> Directory
pkgDocsDir projName pkgName = pkgBuildDir projName pkgName </> "docs"

-- | Directory where a dependency is downloaded to.
repoDir :: ProjectName -> Directory
repoDir (ProjectName projName) =
  buildDir </> "src" </> T.unpack projName

-- | Location of project file in a downloaded project (dependency).
repoDirProjFile :: ProjectName -> FilePath
repoDirProjFile projName = repoDir projName </> projectFile

-- | File which contains project information.
projectFile :: FilePath
projectFile = "idr-project.json"

-- | File which contains package information.
pkgFile :: FilePath
pkgFile = "idr-package.json"

-- | File which contains package set information.
pkgSetFile :: FilePath
pkgSetFile = "idr-package-set.json"

-- | Location of the generated .ipkg file.
ipkgFile :: ProjectName -> PackageName -> FilePath
ipkgFile projName pkgName@(PackageName name) =
  pkgBuildDir projName pkgName </> T.unpack name <> ".ipkg"

-- | Location of the dependency graph file.
depGraphFile :: FilePath
depGraphFile = buildDir </> "dependency-graph.json"

