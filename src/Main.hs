
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Main where

-- Imports

import Control.Monad.Reader
import Control.Monad.Logger
import Data.Default (def)
import Data.Aeson (eitherDecode)
import System.IO.Error
import System.Directory
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BSL
import Idream.OptionParser
import Idream.Types
import Idream.Command.Fetch (fetchDeps)
import Idream.Command.Compile (compileCode)
import Idream.Command.Clean (cleanCode)
import Idream.Command.Run (runCode)
import Idream.Command.Repl (startRepl)
import Idream.Command.New (startNewProject)
import Idream.Command.Validate (validateConfig)
import Idream.Command.MkDoc (generateDocs)
import Idream.Command.GenerateIpkg (generateIpkgFile)
import Idream.Command.Test (runTests)


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

-- | Helper function to read build settings from ".idream" file in a package.
readBuildSettings :: IO BuildSettings
readBuildSettings =
  let defaultSettings = const def
  in flip catchIOError defaultSettings $ do
    cwd <- getCurrentDirectory
    jsonContents <- BSL.readFile $ cwd </> ".idream"
    either defaultSettings return $ eitherDecode jsonContents

-- | Function that processes the given command.
processCommand :: (MonadReader Config m,
                   MonadLogger m,
                   MonadIO m)
               => Command
               -> m ()
processCommand Fetch = fetchDeps
processCommand Compile = compileCode
processCommand Clean = cleanCode
processCommand (Run runArgs) = runCode runArgs
processCommand Repl = startRepl
processCommand (New pkgName pkgType) = startNewProject pkgName pkgType
processCommand Validate = validateConfig
processCommand MkDoc = generateDocs
processCommand GenerateIpkg = generateIpkgFile
processCommand Test = runTests
