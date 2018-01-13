

module Idream.OptionParser ( PkgName
                           , Argument
                           , CodeType(..)
                           , LogLevel(..)
                           , Command(..)
                           , Args(..)
                           , parseCmdLineArgs
                           ) where

-- Imports

import Options.Applicative
import Data.Text (Text)
import Data.Semigroup ((<>))


-- Data types

-- | Type alias for name of a package.
type PkgName = Text

-- | Type alias for command line arguments passed to the run command.
type Argument = Text

-- | Helper type for indicating the type of a package.
data CodeType = Library
              | Executable
              deriving (Eq, Show)

-- | Log level to be used while using idream.
data LogLevel = Info
              | Debug
              deriving (Eq, Show)

-- | Type describing the various commands that can be passed in via the commandline interface.
data Command = Fetch                 -- ^ Fetches all dependencies as described in json file
             | Compile               -- ^ Compiles all code (fetch needs to be done first to get dependencies code?)
             | Clean                 -- ^ Cleans up build artifacts, fetched code?
             | Run [Argument]        -- ^ Runs the executable defined in idr-project.json
             | Repl                  -- ^ Opens up the repl
             -- following are more 'optional', maybe for later?
             | New PkgName CodeType  -- ^ Initializes a new project built on the idream structure
             | Validate              -- ^ Validates the JSON files used by idream
             | MkDoc                 -- ^ Generates the documentation
             | GenerateIpkg          -- ^ Generates a ipkg file from the idream JSON files
             | Test                  -- ^ Runs unit tests for this project
             deriving (Eq, Show)

-- | Data structure representing the arguments passed in to the program.
data Args = Args LogLevel Command deriving (Eq, Show)


-- Functions

-- | Helper function for parsing the log level the build tool should use.
logLevelParser :: Parser LogLevel
logLevelParser = option (eitherReader f) desc where
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
          <> mkCmd "validate" (pure Validate) "Validates the JSON config files used by idream."
          <> mkCmd "docs" (pure MkDoc) "Generates the documentation."
          <> mkCmd "generate-ipkg" (pure GenerateIpkg) "Generates an ipkg file from the Idream JSON files."
          <> mkCmd "test" (pure Test) "Runs unit tests for this project."
  mkCmd name parser desc = command name (info parser (progDesc desc))
  runParser = Run <$> many (strArgument (metavar ""))
  newCmdParser = New <$> (strArgument (metavar "")) <*> codeTypeParser
  codeTypeParser =  flag' Library (long "--lib")
                <|> flag' Executable (long "--exe")

-- | Helper function for parsing the command line arguments.
argsParser :: Parser Args
argsParser = Args <$> logLevelParser <*> commandParser

-- | Function that performs the parsing of command line arguments.
parseCmdLineArgs :: IO Args
parseCmdLineArgs = execParser opts where
  opts = info (argsParser <**> helper) desc
  desc = fullDesc <> progDesc "A simple build system for Idris."
