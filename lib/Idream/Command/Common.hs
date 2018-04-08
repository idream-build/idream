
module Idream.Command.Common ( setupBuildDir
                             , readProjFile
                             , readRootProjFile
                             , readPkgFile
                             , getPkgDirPath
                             , getPkgFilePath
                             , safeCreateDir
                             , safeWriteFile
                             , checkDirExists
                             , checkFileExists
                             , invokeCmdWithEnv
                             , handleReadProjectErr
                             , handleReadPkgErr
                             , Command
                             , Arg
                             , Environment
                             , ReadPkgErr(..)
                             , ReadProjectErr(..)
                             ) where

-- TODO move to Idream.Helpers?
-- Imports

import qualified Idream.Log as Log
import Idream.Log ( MonadLogger )
import Idream.SafeIO
import Idream.Types ( Project(..), ProjectName(..), Package(..), PackageName(..) )
import Idream.FileSystem
import Control.Exception ( IOException )
import System.Directory ( createDirectory, createDirectoryIfMissing
                        , doesDirectoryExist, doesFileExist )
import System.FilePath ( (</>) )
import System.Process ( createProcess, waitForProcess, proc, env )
import System.Exit ( ExitCode(..) )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson ( eitherDecode )
import Data.Monoid ( (<>) )


-- Data types


-- | Type alias for command when spawning an external OS process.
type Command = String

-- | Type alias for command line arguments when spawning an external OS process.
type Arg = String

-- | Type alias for an environment to be passed to a command,
--   expressed as a list of key value pairs.
type Environment = [(String, String)]

-- | Error type used for describing errors that can occur while reading out the project file.
data ReadProjectErr = ProjectFileNotFound IOException
                    | ProjectParseErr String
                    deriving (Eq, Show)

-- | Error type used for describing errors that can occur while reading out a package file.
data ReadPkgErr = PkgFileNotFound IOException
                | PkgParseErr String
                deriving (Eq, Show)


-- Functions

-- | Creates a build directory in which idream will store all build artifacts.
setupBuildDir :: MonadSafeIO e m => (IOException -> e) -> m ()
setupBuildDir f = liftSafeIO f $ createDirectoryIfMissing True buildDir

-- | Reads out a project file (idr-project.json).
readProjFile :: MonadSafeIO e m => (ReadProjectErr -> e) -> FilePath -> m Project
readProjFile f file = do
  projectJSON <- liftSafeIO (f . ProjectFileNotFound) $ BSL.readFile file
  either (raiseError . f . ProjectParseErr) return $ eitherDecode projectJSON

-- | Reads out the top level project file (idr-project.json).
readRootProjFile :: MonadSafeIO e m => (ReadProjectErr -> e) -> m Project
readRootProjFile f = readProjFile f projectFile

-- | Reads out a package file (idr-package.json)
readPkgFile :: MonadSafeIO e m => (ReadPkgErr -> e) -> FilePath -> m Package
readPkgFile f file = do
  pkgJSON <- liftSafeIO (f . PkgFileNotFound) $ BSL.readFile file
  either (raiseError . f . PkgParseErr) return $ eitherDecode pkgJSON

-- Helper function to determine location of package directory.
getPkgDirPath :: MonadSafeIO e m
              => (ReadProjectErr -> e)
              -> PackageName -> ProjectName -> m Directory
getPkgDirPath f pkg@(PackageName pkgName) (ProjectName projName) = do
  Project _ rootPkgNames <- readRootProjFile f
  let basePath = if pkg `elem` rootPkgNames
                   then "."
                   else buildDir </> "src" </> T.unpack projName
  return $ basePath </> T.unpack pkgName

-- Helper function to determine location of package file.
getPkgFilePath :: MonadSafeIO e m
               => (ReadProjectErr -> e)
               -> PackageName -> ProjectName -> m FilePath
getPkgFilePath f pkgName projName =
  (</> pkgFile) <$> getPkgDirPath f pkgName projName

-- | Safely creates a directory while handling possible exceptions.
safeCreateDir :: MonadSafeIO e m => (IOException -> e) -> Directory -> m ()
safeCreateDir f dir = liftSafeIO f $ createDirectory dir

-- | Safely writes to a file, while handling possible exceptions.
safeWriteFile :: MonadSafeIO e m => (IOException -> e) -> T.Text -> FilePath -> m ()
safeWriteFile f txt path = liftSafeIO f $ TIO.writeFile path txt

-- | Helper function for checking if a directory exists.
checkDirExists :: MonadSafeIO e m => (IOException -> e) -> Directory -> m Bool
checkDirExists f dir = liftSafeIO f $ doesDirectoryExist dir

-- | Helper function for checking if a file exists.
checkFileExists :: MonadSafeIO e m => (IOException -> e) -> FilePath -> m Bool
checkFileExists f path = liftSafeIO f $ doesFileExist path


-- | Invokes a command as a separate operating system process.
--   Allows passing additional environment variables to the external process.
invokeCmdWithEnv :: MonadSafeIO e m
                 => (IOException -> e)
                 -> Command -> [Arg] -> Environment
                 -> m ExitCode
invokeCmdWithEnv f cmd cmdArgs environ = liftSafeIO f $ do
  let process = (proc cmd cmdArgs) { env = Just environ }
  (_, _, _, procHandle) <- createProcess process
  waitForProcess procHandle

-- | Helper function for handling errors related to
--   readout of project file.
handleReadProjectErr :: MonadLogger m => ReadProjectErr -> m ()
handleReadProjectErr (ProjectFileNotFound err) =
  Log.err (T.pack $ "Did not find project file: " <> show err <> ".")
handleReadProjectErr (ProjectParseErr err) =
  Log.err (T.pack $ "Failed to parse project file: " <> err <> ".")

-- | Helper function for handling errors related to
--   readout of package file.
handleReadPkgErr :: MonadLogger m => ReadPkgErr -> m ()
handleReadPkgErr (PkgFileNotFound err) =
  Log.err (T.pack $ "Failed to read package file: " <> show err <> ".")
handleReadPkgErr (PkgParseErr err) =
  Log.err (T.pack $ "Failed to parse package file: " <> err <> ".")

