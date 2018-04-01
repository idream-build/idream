
{-# LANGUAGE FlexibleContexts #-}

module Idream.Command.Common ( setupBuildDir
                             , readProjFile
                             , readPkgFile
                             , safeCreateDir
                             , safeWriteFile
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

import Control.Monad.Reader
import qualified Idream.Log as Log
import Idream.Log ( MonadLogger )
import Idream.SafeIO
import Control.Exception ( IOException )
import System.Directory ( createDirectory
                        , createDirectoryIfMissing
                        , getCurrentDirectory )
import System.FilePath ( FilePath, (</>) )
import System.Process ( createProcess, waitForProcess, proc, env )
import System.Exit ( ExitCode(..) )
import Idream.Types ( Config(..), Project(..), Package(..), Directory, buildDir )
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
setupBuildDir :: ( MonadReader Config m, MonadSafeIO e m )
              => (IOException -> e) -> m ()
setupBuildDir f = do
  buildDirectory <- asks $ buildDir . buildSettings
  liftSafeIO f $ createDirectoryIfMissing True buildDirectory

-- | Reads out a project file (idr-project.json).
readProjFile :: MonadSafeIO e m => (ReadProjectErr -> e) -> FilePath -> m Project
readProjFile f file = do
  projectJSON <- liftSafeIO (f . ProjectFileNotFound) $ do
    dir <- getCurrentDirectory
    BSL.readFile $ dir </> file
  either (raiseError . f . ProjectParseErr) return $ eitherDecode projectJSON

-- | Reads out a package file (idr-package.json)
readPkgFile :: ( MonadLogger m,  MonadSafeIO e m )
            => (ReadPkgErr -> e) -> FilePath -> m Package
readPkgFile f file = do
  pkgJSON <- liftSafeIO (f . PkgFileNotFound) $ do
    dir <- getCurrentDirectory
    BSL.readFile $ dir </> file
  either (raiseError . f .PkgParseErr) return $ eitherDecode pkgJSON

-- | Safely creates a directory while handling possible exceptions.
safeCreateDir :: MonadSafeIO e m => (IOException -> e) -> Directory -> m ()
safeCreateDir f dir = liftSafeIO f $ createDirectory dir

-- | Safely writes to a file, while handling possible exceptions.
safeWriteFile :: MonadSafeIO e m => (IOException -> e) -> T.Text -> FilePath -> m ()
safeWriteFile f txt path = liftSafeIO f $ TIO.writeFile path txt

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

