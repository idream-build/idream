

module Idream.OptionParser ( parseCmdLineArgs ) where

-- Imports

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import Idream.Types ( LogLevel(..)
                    , Command(..)
                    , PackageName(..)
                    , PackageType(..)
                    , Args(..))


-- Functions

-- | Helper function for parsing the log level the build tool should use.
logLevelParser :: Parser LogLevel
logLevelParser = fromMaybe Info <$> optional (option (eitherReader f) desc) where
  desc = long "log-level" <> help "Logging level to be used."
  f "info" = Right Info
  f "debug" = Right Debug
  f _ = Left "Only supported options for log level are 'debug' and 'info'."

-- | Helper function for parsing the commands passed to the build tool.
commandParser :: Parser Command
commandParser = hsubparser commands where
  commands = mkCmd "fetch" (pure Fetch) "Fetches all dependencies."
          <> mkCmd "compile" (pure Compile) "Compiles all code and its dependencies."
          <> mkCmd "clean" (pure Clean) "Cleans up build artifacts and fetched code."
          <> mkCmd "run" runParser "Runs the executable (only valid for executable packages)."
          <> mkCmd "repl" (pure Repl) "Starts the Idris repl."
          <> mkCmd "new" newCmdParser "Initializes a new project."
          <> mkCmd "docs" (pure MkDoc) "Generates the documentation."
          <> mkCmd "generate-ipkg" (pure GenerateIpkg) "Generates an ipkg file from the Idream JSON files."
          <> mkCmd "test" (pure Test) "Runs unit tests for this project."
  mkCmd name parser desc = command name (info parser (progDesc desc))
  runParser = Run <$> many (strArgument (metavar ""))
  newCmdParser = New <$> (PackageName <$> strArgument (metavar ""))
                     <*> codeTypeParser
  codeTypeParser =  flag' Library (long "lib")
                <|> flag' Executable (long "exe")

-- | Helper function for parsing the command line arguments.
argsParser :: Parser Args
argsParser = Args <$> logLevelParser <*> commandParser

-- | Function that performs the parsing of command line arguments.
parseCmdLineArgs :: IO Args
parseCmdLineArgs = execParser opts where
  opts = info (argsParser <**> helper) desc
  desc = fullDesc <> progDesc "A simple build system for Idris."
