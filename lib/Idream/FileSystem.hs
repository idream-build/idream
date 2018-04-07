
module Idream.FileSystem ( module System.FilePath
                         , Directory
                         , buildDir
                         , repoDir
                         , projectFile
                         , pkgFile
                         , pkgSetFile
                         , depGraphFile
                         ) where

-- Imports

import System.FilePath ( FilePath, (</>) )
import Idream.Types ( ProjectName(..) )
import qualified Data.Text as T


-- Data types

-- | Type alias for directories.
type Directory = FilePath


-- Functions

buildDir :: Directory
buildDir = ".idream-work"

projectFile :: FilePath
projectFile = "idr-project.json"

pkgFile :: FilePath
pkgFile = "idr-package.json"

pkgSetFile :: FilePath
pkgSetFile = "idr-package-set.json"

depGraphFile :: FilePath
depGraphFile = buildDir </> "dependency-graph.json"

repoDir :: ProjectName -> Directory
repoDir (ProjectName projName) =
  buildDir </> "src" </> T.unpack projName

