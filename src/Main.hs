
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Main where

-- Imports

import Control.Monad.Reader
import Control.Monad.Logger
import Data.Default (def)
import Data.Aeson (eitherDecode)
import Idream.OptionParser
import Idream.Types
import System.IO.Error
import System.Directory
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BSL


-- Data types

data Config = Config { args :: Args
                     , buildSettings :: BuildSettings
                     } deriving (Eq, Show)


-- Constants

buildSettingsFile :: String
buildSettingsFile = ".idream"


-- Functions

-- | Main function.
main :: IO ()
main = do
  settings <- readBuildSettings
  cmdLineArgs <- parseCmdLineArgs
  let config = Config cmdLineArgs settings
      getLogThreshold Info = LevelInfo
      getLogThreshold _ = LevelDebug
      logThreshold = getLogThreshold $ logLevel cmdLineArgs
  runStdoutLoggingT $ filterLogger (\_ lvl -> lvl >= logThreshold)
                    $ flip runReaderT config $ do
    command <- asks $ cmd . args
    processCommand command


-- | Helper function to read build settings from ".idream file in a package."
readBuildSettings :: IO BuildSettings
readBuildSettings =
  let defaultSettings = const def
  in flip catchIOError defaultSettings $ do
    cwd <- getCurrentDirectory
    jsonContents <- BSL.readFile $ cwd </> buildSettingsFile
    either defaultSettings return $ eitherDecode jsonContents

-- Function that processes the given command.
processCommand :: (MonadReader Config m,
                   MonadLogger m,
                   MonadIO m)
               => Command
               -> m ()
processCommand Fetch = return ()
processCommand Compile = return ()
processCommand (Run runArgs) = return ()
processCommand Repl = return ()
processCommand (New pkgName pkgType) = return ()
processCommand Validate = return ()
processCommand MkDoc = return ()
processCommand GenerateIpkg = return ()
processCommand Test = return ()
