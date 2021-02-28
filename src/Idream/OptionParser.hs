module Idream.OptionParser
  ( parseCmdLineArgs
  ) where

import Data.Maybe (fromMaybe)
import Idream.Types.Command (Args (..), Command (..))
import Idream.Types.Common (PackageName (..), PackageType (..), ProjectName (..))
import LittleLogger (Severity (..))
import Options.Applicative

-- | Helper function for parsing the log level the build tool should use.
logLevelParser :: Parser Severity
logLevelParser = fromMaybe Info <$> optional (option (eitherReader f) desc) where
  desc = long "log-level" <> help "Logging level to be used."
  f "debug" = Right Debug
  f "info" = Right Info
  f "warning" = Right Warning
  f "error" = Right Error
  f _ = Left "Only supported options for log level are 'debug', 'info', 'warning', and 'error'."

-- | Helper function for parsing the commands passed to the build tool.
commandParser :: Parser Command
commandParser = hsubparser commands where
  commands = mkCmd "fetch" (pure Fetch) "Fetches all dependencies."
          <> mkCmd "compile" (pure Compile) "Compiles all code and its dependencies."
          <> mkCmd "clean" (pure Clean) "Cleans up build artifacts and fetched code."
          <> mkCmd "run" runParser "Runs the executable (only valid for executable packages)."
          <> mkCmd "repl" replParser "Starts the Idris repl."
          <> mkCmd "new" newCmdParser "Initializes a new project."
          <> mkCmd "add" addCmdParser "Adds a package to an existing idream project."
          <> mkCmd "mkdocs" (pure MkDoc) "Generates the documentation."
          <> mkCmd "generate-ipkg" (pure GenerateIpkg) "Generates an ipkg file from the Idream JSON files."
          <> mkCmd "test" (pure Test) "Runs unit tests for this project."
  mkCmd name parser desc = command name (info parser (progDesc desc))
  runParser = Run <$> many (strArgument (metavar "ARGS"))
  replParser = Repl <$> (ProjectName <$> strArgument (metavar "PROJECT_NAME"))
                    <*> (PackageName <$> strArgument (metavar "PACKAGE_NAME"))
  newCmdParser = New <$> (ProjectName <$> strArgument (metavar "PROJECT_NAME"))
  addCmdParser = Add <$> (PackageName <$> strArgument (metavar "PACKAGE_NAME"))
                     <*> codeTypeParser
  codeTypeParser =  flag' PkgTypeLibrary (long "lib")
                <|> flag' PkgTypeExecutable (long "exe")
                <|> flag' PkgTypeTest (long "test")

-- | Helper function for parsing the command line arguments.
argsParser :: Parser Args
argsParser = Args <$> logLevelParser <*> commandParser

-- | Function that performs the parsing of command line arguments.
parseCmdLineArgs :: IO Args
parseCmdLineArgs = execParser opts where
  opts = info (argsParser <**> helper) desc
  desc = fullDesc <> progDesc "A simple build system for Idris."
