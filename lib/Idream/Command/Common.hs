
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Idream.Command.Common ( setupBuildDir
                             , readPkgFile
                             , tryAction
                             , invokeCmdWithEnv
                             , handleReadPkgErr
                             , Command
                             , Arg
                             , Environment
                             , ReadPkgErr(..)
                             ) where

-- TODO move to Idream.Helpers?
-- Imports

import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Except
import Control.Exception ( Exception, IOException, try )
import System.Directory ( createDirectoryIfMissing, getCurrentDirectory )
import System.FilePath ( FilePath, (</>) )
import System.Process ( createProcess, waitForProcess, proc, env )
import System.Exit ( ExitCode(..) )
import Idream.Types ( Config(..), Package(..), buildDir )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
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

-- | Error type used for describing errors that can occur while reading out a package file.
data ReadPkgErr = PkgFileNotFound IOException
                | PkgParseErr String
                deriving (Eq, Show)

-- Functions

-- | Creates a build directory in which idream will store all build artifacts.
setupBuildDir :: ( MonadReader Config m, MonadIO m ) => m ()
setupBuildDir = do
  buildDirectory <- asks $ buildDir . buildSettings
  liftIO $ createDirectoryIfMissing True buildDirectory

-- | Reads out a package file (idr-package.json)
readPkgFile :: ( MonadError ReadPkgErr m
               , MonadLogger m
               , MonadIO m )
            => FilePath -> m Package
readPkgFile file = do
  pkgJSON <- tryAction PkgFileNotFound $ do
    dir <- getCurrentDirectory
    BSL.readFile $ dir </> file
  either (throwError . PkgParseErr) return $ eitherDecode pkgJSON

-- | Helper function for running an IO action in a monad transformer stack,
--   while catching possible exceptions and wrapping them in a custom error type.
tryAction :: ( MonadError e' m, MonadIO m, Exception e )
          => (e -> e') -> IO a -> m a
tryAction f = (>>= either (throwError . f) return) <$> liftIO . try

-- | Invokes a command as a separate operating system process.
--   Allows passing additional environment variables to the external process.
invokeCmdWithEnv :: MonadIO m => Command -> [Arg] -> Environment -> m ExitCode
invokeCmdWithEnv cmd cmdArgs environ =
  liftIO $ do
    let process = (proc cmd cmdArgs) { env = Just environ }
    (_, _, _, procHandle) <- createProcess process
    waitForProcess procHandle

-- | Helper function for handling errors related to
--   readout of package file.
handleReadPkgErr :: MonadLogger m => ReadPkgErr -> m ()
handleReadPkgErr (PkgFileNotFound err) =
  $(logError) (T.pack $ "Failed to read package file: " <> show err <> ".")
handleReadPkgErr (PkgParseErr err) =
  $(logError) (T.pack $ "Failed to parse package file: " <> err <> ".")
