
{-# LANGUAGE RankNTypes #-}

module Idream.FileSystem ( FilePath, Directory
                         , FSError(..), FileSystem(..)
                         , runFS, handleFSErr
                         , writeFile, readFile
                         , doesFileExist, doesDirExist
                         , createDir, removePath
                         , copyDir, findFiles
                         , pkgDir, pkgSrcDir
                         , buildDir, projectBuildDir
                         , pkgBuildDir, pkgBuildSrcDir
                         , pkgCompileDir, pkgDocsDir
                         , repoDir
                         , projectFile, repoDirProjFile
                         , pkgFile, pkgSetFile
                         , ipkgFile, depGraphFile
                         ) where

-- Imports

import Prelude hiding ( writeFile, readFile )
import System.FilePath ( FilePath, (</>) )
import qualified System.Directory as Dir
import Idream.Types ( ProjectName(..), PackageName(..), SourceDir(..) )
import Idream.SafeIO
import Data.Monoid ( (<>) )
import Data.Maybe ( fromMaybe )
import Control.Monad.Freer
import Control.Exception ( IOException )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Shelly ( shelly, silently, cp_r, find
              , fromText, toTextIgnore )


-- Data types

-- | Type alias for directories.
type Directory = FilePath

-- | Data type describing possible errors when interacting with the filesystem.
data FSError = WriteFileErr FilePath IOException
             | ReadFileErr FilePath IOException
             | CheckFileExistsErr FilePath IOException
             | CreateDirErr Directory IOException
             | RemovePathErr FilePath IOException
             | CheckDirExistsErr Directory IOException
             | CopyDirErr Directory Directory IOException
             | FindFilesErr Directory IOException
             deriving (Eq, Show)

-- | Data type describing possible interactions with the filesystem.
data FileSystem a where
  WriteFile :: T.Text -> FilePath -> FileSystem ()
  ReadFile :: FilePath -> FileSystem T.Text
  DoesFileExist :: FilePath -> FileSystem Bool
  CreateDir :: Directory -> FileSystem ()
  RemovePath :: FilePath -> FileSystem ()
  DoesDirExist :: Directory -> FileSystem Bool
  CopyDir :: Directory -> Directory -> FileSystem ()
  FindFiles :: (FilePath -> Bool) -> Maybe Directory -> FileSystem [FilePath]


-- Functions

writeFile :: Member FileSystem r => T.Text -> FilePath -> Eff r ()
writeFile txt path = send $ WriteFile txt path

readFile :: Member FileSystem r => FilePath -> Eff r T.Text
readFile path = send $ ReadFile path

doesFileExist :: Member FileSystem r => FilePath -> Eff r Bool
doesFileExist path = send $ DoesFileExist path

createDir :: Member FileSystem r => Directory -> Eff r ()
createDir dir = send $ CreateDir dir

removePath :: Member FileSystem r => FilePath -> Eff r ()
removePath path = send $ RemovePath path

doesDirExist :: Member FileSystem r => Directory -> Eff r Bool
doesDirExist dir = send $ DoesDirExist dir

copyDir :: Member FileSystem r => Directory -> Directory -> Eff r ()
copyDir from to = send $ CopyDir from to

findFiles :: Member FileSystem r
          => (FilePath -> Bool) -> Maybe Directory -> Eff r [FilePath]
findFiles f dir = send $ FindFiles f dir

runFS :: forall e r. LastMember (SafeIO e) r
      => (FSError -> e) -> Eff (FileSystem ': r) ~> Eff r
runFS f = interpretM g where
  g :: FileSystem ~> SafeIO e
  g (WriteFile txt path) = liftSafeIO (f . WriteFileErr path)
                         $ TIO.writeFile path txt
  g (ReadFile path) = liftSafeIO (f . ReadFileErr path) $ TIO.readFile path
  g (DoesFileExist path) = liftSafeIO (f . CheckFileExistsErr path)
                         $ Dir.doesFileExist path
  g (CreateDir dir) = liftSafeIO (f . CreateDirErr dir)
                    $ Dir.createDirectoryIfMissing True dir
  g (RemovePath path) = liftSafeIO (f . RemovePathErr path)
                      $ Dir.removePathForcibly path
  g (DoesDirExist dir) = liftSafeIO (f . CheckDirExistsErr dir)
                       $ Dir.doesDirectoryExist dir
  g (CopyDir from to) =
    let toFilePath = fromText . T.pack
     in liftSafeIO (f . CopyDirErr from to) $
        shelly $ silently $ cp_r (toFilePath from) (toFilePath to)
  g (FindFiles h dir) =
    let toFilePath = fromText . T.pack
        fromFilePath = T.unpack . toTextIgnore
        dir' = fromMaybe "." dir
     in liftSafeIO (f . FindFilesErr dir') $ do
       files <- shelly . silently . find . toFilePath $ dir'
       return . filter h $ fromFilePath <$> files


handleFSErr :: FSError -> T.Text
handleFSErr (WriteFileErr path err) =
  T.pack $ "Failed to write to file (" <> path <> "): " <> show err <> "."
handleFSErr (ReadFileErr path err) =
  T.pack $ "Failed to read from file (" <> path <> "): " <> show err <> "."
handleFSErr (CheckFileExistsErr path err) =
  T.pack $ "Failed to check if file exists (" <> path <> "): " <> show err <> "."
handleFSErr (CreateDirErr dir err) =
  T.pack $ "Failed to create directory (" <> dir <> "): " <> show err <> "."
handleFSErr (RemovePathErr path err) =
  T.pack $ "Failed to remove path (" <> path <> "): " <> show err <> "."
handleFSErr (CheckDirExistsErr dir err) =
  T.pack $ "Failed to check if directory exists (" <> dir <> "): " <> show err <> "."
handleFSErr (CopyDirErr from to err) =
  T.pack $ "Failed to copy files from " <> from <> " to " <> to <> ", reason: " <> show err <> "."
handleFSErr (FindFilesErr dir err) =
  T.pack $ "Error when trying to find files (" <> dir <> "): " <> show err <> "."


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

-- | File which contains project information.
projectFile :: FilePath
projectFile = "idr-project.json"

-- | Location of project file in a downloaded project (dependency).
repoDirProjFile :: ProjectName -> FilePath
repoDirProjFile projName = repoDir projName </> projectFile

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

