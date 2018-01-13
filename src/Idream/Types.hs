
{-# LANGUAGE OverloadedStrings #-}

module Idream.Types ( BuildSettings(..)
                    , PackageName(..)
                    , PkgName
                    , Argument
                    , SourceDir(..)
                    , Repo(..)
                    , Version(..)
                    , Project(..)
                    , Package(..)
                    , PackageDescr(..)
                    , PackageSet(..)
                    , LogLevel(..)
                    , CodeType(..)
                    , Command(..)
                    , Args(..)
                    , Config(..)
                    ) where


-- Imports

import Data.Default
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import Data.Maybe (maybe)
import Control.Monad (mzero)


-- Data types

type Directory = FilePath
data BuildSettings = BuildSettings { cacheDir :: Directory
                                   , projectFile :: FilePath
                                   } deriving (Eq, Show)

newtype PackageName = PackageName Text deriving (Eq, Show)
newtype SourceDir = SourceDir Text deriving (Eq, Show)
newtype Repo = Repo Text deriving (Eq, Show)
newtype Version = Version Text deriving (Eq, Show)

newtype Project = Project [PackageName] deriving (Eq, Show)
data Package = PkgLibrary PackageName (Maybe SourceDir) [PackageName]
             | PkgExecutable PackageName (Maybe SourceDir) [PackageName]
data PackageDescr = PackageDescr Repo Version (Maybe SourceDir)
newtype PackageSet = PackageSet (Map Text PackageDescr)


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
data Args = Args { logLevel :: LogLevel
                 , cmd :: Command } deriving (Eq, Show)

data Config = Config { args :: Args
                     , buildSettings :: BuildSettings
                     } deriving (Eq, Show)



-- Instances

instance Default BuildSettings where
  def = BuildSettings ".idream-work/" "idr-project.json"

instance FromJSON BuildSettings where
  parseJSON (Object o) = BuildSettings
                      <$> o .: "cache-dir"
                      <*> o .: "project-file"
  parseJSON _ = mzero

instance FromJSON PackageName where
  parseJSON v = PackageName <$> parseJSON v

instance FromJSON SourceDir where
  parseJSON v = SourceDir <$> parseJSON v

instance FromJSON Repo where
  parseJSON v = Repo <$> parseJSON v

instance FromJSON Version where
  parseJSON v = Version <$> parseJSON v

instance FromJSON Project where
  parseJSON (Object o) = Project <$> o .: "packages"
  parseJSON _ = mzero

instance FromJSON Package where
  parseJSON (Object o) = do
    name <- o .: "name"
    srcDir <- o .: "sourcedir"
    pkgs <- o .:? "packages"
    case pkgs of
      Nothing -> do
        modules <- o .:? "modules"
        return $ PkgLibrary name srcDir (maybe [] id modules)
      Just pkgs' -> return $ PkgExecutable name srcDir pkgs'
  parseJSON _ = mzero

instance FromJSON PackageDescr where
  parseJSON (Object o) =
    PackageDescr <$> o .: "repo"
                 <*> o .: "version"
                 <*> o .:? "subdir"
  parseJSON _ = mzero

instance FromJSON PackageSet where
  parseJSON v = PackageSet <$> parseJSON v
