
{-# LANGUAGE FlexibleContexts #-}

module Main where

-- Imports

import Control.Monad.Reader
import Control.Monad.Logger
import Data.Default (def)
import Idream.OptionParser
import Idream.Types
import Idream.Command.Fetch (fetchDeps)
import Idream.Command.Compile (compileCode)
import Idream.Command.Clean (cleanCode)
import Idream.Command.Run (runCode)
import Idream.Command.Repl (startRepl)
import Idream.Command.New (startNewProject)
import Idream.Command.MkDoc (generateDocs)
import Idream.Command.GenerateIpkg (generateIpkgFile)
import Idream.Command.Test (runTests)


-- Functions

-- | Main function.
main :: IO ()
main = do
  cmdLineArgs <- parseCmdLineArgs
  let config = Config cmdLineArgs def
      getLogThreshold Info = LevelInfo
      getLogThreshold _ = LevelDebug
      logThreshold = getLogThreshold $ logLevel cmdLineArgs
  runStdoutLoggingT $ filterLogger (\_ lvl -> lvl >= logThreshold)
                    $ flip runReaderT config $ do
    command <- asks $ cmd . args
    processCommand command

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
processCommand MkDoc = generateDocs
processCommand GenerateIpkg = generateIpkgFile
processCommand Test = runTests
