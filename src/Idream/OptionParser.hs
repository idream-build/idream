

module Idream.OptionParser ( CacheDir
                           , ProjectFile
                           , PackageName
                           , Argument
                           , LogLevel(..)
                           , FetchFlag (..)
                           , GenerateIpkgFlag (..)
                           , Op(..)
                           , Args(..)
                           , parseCmdLineArgs
                           ) where

-- Imports

import Options.Applicative
import Data.Text (Text)
import Data.Semigroup ((<>))


-- Data types

type CacheDir = Text
type ProjectFile = Text
type PackageName = Text
type Argument = Text

data LogLevel = Info | Debug deriving (Eq, Show)
data FetchFlag = NoFetch | ForceFetch deriving (Eq, Show)
data GenerateIpkgFlag = DontGenerateIpkg | GenerateIpkg deriving (Eq, Show)

data Op = Nuke
        | Validate
        | Generate PackageName
        | Fetch PackageName FetchFlag
        | Execute PackageName [Argument]
        | Build PackageName GenerateIpkgFlag FetchFlag
        | Repl PackageName GenerateIpkgFlag FetchFlag
        | MkDoc PackageName GenerateIpkgFlag FetchFlag
        | CheckPkg PackageName GenerateIpkgFlag FetchFlag
        deriving (Eq, Show)

data Args = Args { cacheDir :: CacheDir
                 , projectFile :: ProjectFile
                 , logLevel :: LogLevel
                 , op :: Op
                 } deriving (Eq, Show)

-- Functions

logLevelParser :: Parser LogLevel
logLevelParser = option (eitherReader f) desc where
  desc = long "log-level" <> help "Logging level."
  f "info" = Right Info
  f "debug" = Right Debug
  f _ = Left "Only supported options for log level are 'debug' and 'info'."

opParser :: Parser Op
opParser = hsubparser ops where
  ops = mkCmd "nuke" (pure Nuke) "Remove cache."
     <> mkCmd "validate" (pure Validate) "Validate configuration."
     <> mkCmd "generate" generateParser "Generate ipkg."
     <> mkCmd "fetch" fetchParser "Fetch an external dependency."
     <> mkCmd "execute" executeParser "Runs a binary."
     <> mkGenCmd "build" Build
     <> mkGenCmd "repl" Repl
     <> mkGenCmd "mkdoc" MkDoc
     <> mkGenCmd "checkpkg" CheckPkg
  mkCmd name parser desc = command name (info parser (progDesc desc))
  mkGenCmd name f = mkCmd name (genParser f) ("Invokes idris --" <> name <> ".")
  generateParser = Generate <$> strOption (long "name" <> help "Name of local package to generate.")
  fetchParser = Fetch <$> strOption (long "name" <> help "Name of package in package set.")
                      <*> flag NoFetch ForceFetch
                               (long "force-fetch" <> help "Force redownload")
  executeParser = Execute <$> strOption (long "name" <> help "Name of package to run.")
                          <*> many (strArgument (metavar ""))
  genParser f = f <$> strOption (long "name" <> help "Name of local package (set)")
                  <*> flag GenerateIpkg DontGenerateIpkg
                           (long "no-generate" <> help "Flag for generating ipkg")
                  <*> flag NoFetch ForceFetch
                           (long "force-fetch" <> help "Force download of packages.")

argsParser :: Parser Args
argsParser =
  Args <$> strOption (long "cache" <> help "Cache location")
       <*> strOption (long "project" <> help "Project definitions")
       <*> logLevelParser
       <*> opParser

parseCmdLineArgs :: IO Args
parseCmdLineArgs = execParser opts where
  opts = info (argsParser <**> helper) desc
  desc = fullDesc <> progDesc "A simple build system for Idris."
