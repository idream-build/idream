module Idream.OptionParser
  ( parseCmdLineArgs
  ) where

import Idream.Prelude
import Idream.Types.Command (Args (..), Command (..))
import Idream.Types.Common (PackageGroup (..), PackageName, PackageType (..), ProjectName,
                            RefreshStrategy (..), packageGroupFromList, packageTypeFromText, refreshStratFromText)
import LittleLogger (Severity (..))
import Options.Applicative

-- | Helper function for parsing the log level the build tool should use.
logLevelParser :: Parser Severity
logLevelParser = fromMaybe Info <$> optional (option (eitherReader f) desc) where
  desc = long "log-level" <> help "Logging level (debug/info/warning/error)"
  f "debug" = Right Debug
  f "info" = Right Info
  f "warning" = Right Warning
  f "error" = Right Error
  f _ = Left "log-level must be one of debug/info/warning/error"

projDirParser :: Parser (Maybe Directory)
projDirParser = optional (option str desc) where
  desc = long "project-dir" <> help "Root project directory"

pkgDirParser :: Parser (Maybe Directory)
pkgDirParser = optional (option str desc) where
  desc = long "package-dir" <> help "Package directory"

packageTypeParser :: Parser PackageType
packageTypeParser = fromMaybe PkgTypeLibrary <$> optional (option (eitherReader f) desc) where
  desc = long "package-type" <> help "Package type (library/executable/test)"
  f s = case packageTypeFromText (toText s) of
    Just pt -> Right pt
    Nothing -> Left "package-type must be one of library/executable/test"

packageGroupParser :: Parser PackageGroup
packageGroupParser = packageGroupFromList <$> many (option str desc) where
  desc = long "package"

refreshStratParser :: Parser RefreshStrategy
refreshStratParser = fromMaybe EnableRefresh <$> optional (option (eitherReader f) desc) where
  desc = long "refresh-strategy" <> help "Refresh strategy (force/enable/disable)"
  f s = case refreshStratFromText (toText s) of
    Just pt -> Right pt
    Nothing -> Left "refresh-strategy must be one of force/enable/disable"

-- | Helper function for parsing the commands passed to the build tool.
commandParser :: Parser Command
commandParser = hsubparser commands where
  commands = mkCmd "fetch" fetchCmdParser "Fetches all dependencies."
          <> mkCmd "compile" compileCmdParser "Compiles all code and its dependencies."
          <> mkCmd "clean" cleanCmdParser "Cleans up build artifacts and fetched code."
          <> mkCmd "run" runCmdParser "Runs the executable (only valid for executable or test packages)."
          <> mkCmd "repl" replCmdParser "Starts the Idris repl."
          <> mkCmd "new" newCmdParser "Initializes a new project."
          <> mkCmd "add" addCmdParser "Adds a package to an existing idream project."
          <> mkCmd "test" testCmdParser "Runs unit tests for this project."
  mkCmd name parser desc = command name (info parser (progDesc desc))
  fetchCmdParser = Fetch <$> packageGroupParser <*> refreshStratParser
  compileCmdParser = Compile <$> packageGroupParser
  cleanCmdParser = pure Clean
  runCmdParser = Run <$> (fromText <$> strArgument (metavar "PACKAGE_NAME"))
                     <*> many (strArgument (metavar "ARGS"))
  replCmdParser = Repl <$> (fromText <$> strArgument (metavar "PACKAGE_NAME"))
  newCmdParser = New <$> (fromText <$> strArgument (metavar "PROJECT_NAME"))
  addCmdParser = Add <$> pkgDirParser
                     <*> (fromText <$> strArgument (metavar "PACKAGE_NAME"))
                     <*> packageTypeParser
  testCmdParser = Test <$> packageGroupParser

-- | Helper function for parsing the command line arguments.
argsParser :: Parser Args
argsParser = Args <$> logLevelParser <*> projDirParser <*> commandParser

-- | Function that performs the parsing of command line arguments.
parseCmdLineArgs :: IO Args
parseCmdLineArgs = execParser opts where
  opts = info (argsParser <**> helper) desc
  desc = fullDesc <> progDesc "A simple build system for Idris."
