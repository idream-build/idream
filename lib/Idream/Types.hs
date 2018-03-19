
{-# LANGUAGE OverloadedStrings #-}

module Idream.Types ( BuildSettings(..)
                    , ProjectName(..)
                    , PackageName(..)
                    , Argument
                    , SourceDir(..)
                    , Repo(..)
                    , Version(..)
                    , Project(..)
                    , Package(..)
                    , PackageDescr(..)
                    , PackageSet(..)
                    , LogLevel(..)
                    , PackageType(..)
                    , Command(..)
                    , Args(..)
                    , Config(..)
                    , Directory
                    ) where


-- Imports

import Data.Default
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import Control.Monad (mzero)


-- Data types

-- | Type alias for directories.
type Directory = FilePath

-- | Type used for representing project names.
newtype ProjectName = ProjectName { unProjName :: Text }
                    deriving (Eq, Ord, Show)

-- | Type used for representing package names.
newtype PackageName = PackageName { unPkgName :: Text }
                    deriving (Eq, Ord, Show)

-- | Type used for representing the source directory of a package.
newtype SourceDir = SourceDir Directory deriving (Eq, Show)

-- | Helper type for indicating the type of a package.
data PackageType = Library | Executable deriving (Eq, Show)

-- | Type containing project data (coming from idr-project.json).
--   A project consists of 1 or more packages that are closely related to
--   each other. This can for example be a binary, library, tests, ...
data Project = Project { projProjName :: ProjectName
                       , projDeps :: [PackageName]
                       } deriving (Eq, Show)

-- | Type containing package data (coming from idr-package.json).
--   A package can depend on 1 or more projects (which can possibly contain
--   multiple closely related binaries or libraries).
--   If a package does depend on a project, it will depend on each of the
--   packages in that project.
data Package = Package PackageName PackageType SourceDir [ProjectName]
             deriving (Eq, Show)

-- | Type used for representing repository location of a package.
newtype Repo = Repo Text deriving (Eq, Show)

-- | Type containing version information of a package.
newtype Version = Version Text deriving (Eq, Show)

-- | Type describing a package (dependency).
data PackageDescr = PackageDescr Repo Version
                  deriving (Eq, Show)

-- | Type containing information about a package set
--   (as described in idr-package-set.json).
--   A package set is a collection of git repositories used for finding
--   projects (which contain packages).
newtype PackageSet = PackageSet (Map Text PackageDescr)
                   deriving (Eq, Show)

-- | Type alias for command line arguments passed to the run command.
type Argument = Text

-- | Log level to be used while using idream.
data LogLevel = Info | Debug deriving (Eq, Show)

-- | Type describing the various commands that can be passed in via the commandline interface.
data Command = Fetch                        -- ^ Fetches all dependencies as described in json file
             | Compile                      -- ^ Compiles all code (fetch needs to be done first to get dependencies code?)
             | Clean                        -- ^ Cleans up build artifacts, fetched code?
             | Run [Argument]               -- ^ Runs the executable defined in idr-project.json
             | Repl                         -- ^ Opens up the repl
             | New PackageName PackageType  -- ^ Initializes a new project built on the idream structure
             | MkDoc                        -- ^ Generates the documentation
             | GenerateIpkg                 -- ^ Generates a ipkg file from the idream JSON files
             | Test                         -- ^ Runs unit tests for this project
             deriving (Eq, Show)

-- | Data structure representing the arguments passed in to the program.
data Args = Args { logLevel :: LogLevel
                 , cmd :: Command } deriving (Eq, Show)

-- | Type containing build settings not passing via commandline args.
data BuildSettings = BuildSettings { buildDir :: Directory
                                   , projectFile :: FilePath
                                   , pkgSetFile :: FilePath
                                   , pkgFile :: FilePath
                                   } deriving (Eq, Show)

-- | Type grouping all settings together into 1 big structure.
data Config = Config { args :: Args
                     , buildSettings :: BuildSettings
                     } deriving (Eq, Show)


-- Instances

instance Default BuildSettings where
  def = BuildSettings ".idream-work" "idr-project.json"
                      "idr-package-set.json" "idr-package.json"

instance Default SourceDir where
  def = SourceDir "src"

instance FromJSON PackageName where
  parseJSON v = PackageName <$> parseJSON v

instance ToJSON PackageName where
  toJSON (PackageName v) = toJSON v

instance FromJSON ProjectName where
  parseJSON v = ProjectName <$> parseJSON v

instance ToJSON ProjectName where
  toJSON (ProjectName v) = toJSON v

instance FromJSON SourceDir where
  parseJSON v = SourceDir <$> parseJSON v

instance FromJSON Repo where
  parseJSON v = Repo <$> parseJSON v

instance FromJSON Version where
  parseJSON v = Version <$> parseJSON v

instance FromJSON Project where
  parseJSON (Object o) =
    Project <$> o .: "package_name"
            <*> o .: "packages"
  parseJSON _ = mzero

instance FromJSON Package where
  parseJSON (Object o) = do
    name <- o .: "name"
    srcDir <- o .:? "sourcedir" .!= def
    pkgs <- o .:? "packages" .!= def
    isExecutable <- o .:? "executable" .!= False
    let pkgType = if isExecutable then Executable else Library
    return $ Package name pkgType srcDir pkgs
  parseJSON _ = mzero

instance FromJSON PackageDescr where
  parseJSON (Object o) =
    PackageDescr <$> o .: "repo"
                 <*> o .: "version"
  parseJSON _ = mzero

instance FromJSON PackageSet where
  parseJSON v = PackageSet <$> parseJSON v
