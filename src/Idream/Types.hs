module Idream.Types
  ( ProjectName (..)
  , PackageName (..)
  , Argument
  , SourceDir (..)
  , Repo (..)
  , Version (..)
  , Project (..)
  , Package (..)
  , Dependency (..)
  , PackageDescr (..)
  , PackageSet (..)
  , PackageType (..)
  , Command (..)
  , Args (..)
  ) where

import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), ToJSON (..), Value (..), (.=), (.:), (.!=), (.:?), object)
import Data.Default (Default (..))
import Data.Map (Map)
import Data.Text (Text)
import Idream.ToText (ToText (..))
import LittleLogger (Severity)

-- | Type used for representing project names.
newtype ProjectName = ProjectName { unProjName :: Text }
  deriving (Eq, Ord, Show)

instance FromJSON ProjectName where
  parseJSON v = ProjectName <$> parseJSON v

instance ToJSON ProjectName where
  toJSON (ProjectName v) = toJSON v

instance ToText ProjectName where
  toText = unProjName

-- | Type used for representing package names.
newtype PackageName = PackageName { unPkgName :: Text }
  deriving (Eq, Ord, Show)

instance FromJSON PackageName where
  parseJSON v = PackageName <$> parseJSON v

instance ToJSON PackageName where
  toJSON (PackageName v) = toJSON v

instance ToText PackageName where
  toText = unPkgName

-- | Type used for representing the source directory of a package.
newtype SourceDir = SourceDir FilePath deriving (Eq, Show)

instance Default SourceDir where
  def = SourceDir "src"

instance FromJSON SourceDir where
  parseJSON v = SourceDir <$> parseJSON v

-- | Helper type for indicating the type of a package.
data PackageType = Library | Executable deriving (Eq, Show)

-- | Type containing project data (coming from idr-project.json).
--   A project consists of 1 or more packages that are closely related to
--   each other. This can for example be a binary, library, tests, ...
data Project = Project
  { projProjName :: ProjectName
  , projDeps :: [PackageName]
  } deriving (Eq, Show)

instance FromJSON Project where
  parseJSON (Object o) =
    Project <$> o .: "project_name"
            <*> o .: "packages"
  parseJSON _ = mzero

instance ToJSON Project where
  toJSON (Project projName deps) =
    object [ "project_name" .= toJSON projName
           , "packages" .= toJSON deps ]

-- | Type describing a dependency.
--   For idream, this is the combination of a project and a package inside that project.
data Dependency = Dependency
  { depProjName :: ProjectName
  , depPkgName :: PackageName
  } deriving (Eq, Show)

instance FromJSON Dependency where
  parseJSON (Object o) = do
    proj <- o .: "project"
    pkg <- o .: "package"
    return $ Dependency proj pkg
  parseJSON _ = mzero

instance ToText Dependency where
  toText (Dependency projName pkgName) =
    toText projName <> "_" <> toText pkgName

-- | Type containing package data (coming from idr-package.json).
--   A package can depend on 1 or more projects (which can possibly contain
--   multiple closely related binaries or libraries).
data Package = Package PackageName PackageType SourceDir [Dependency]
  deriving (Eq, Show)

instance FromJSON Package where
  parseJSON (Object o) = do
    name <- o .: "name"
    srcDir <- o .:? "source_dir" .!= def
    pkgs <- o .:? "dependencies" .!= def
    isExecutable <- o .:? "executable" .!= False
    let pkgType = if isExecutable then Executable else Library
    return $ Package name pkgType srcDir pkgs
  parseJSON _ = mzero

-- | Type used for representing repository location of a package.
newtype Repo = Repo Text deriving (Eq, Show)

instance FromJSON Repo where
  parseJSON v = Repo <$> parseJSON v

instance ToText Repo where
  toText (Repo r) = r

-- | Type containing version information of a package.
newtype Version = Version Text deriving (Eq, Show)

instance FromJSON Version where
  parseJSON v = Version <$> parseJSON v

instance ToText Version where
  toText (Version v) = v

-- | Type describing a package (dependency).
data PackageDescr = PackageDescr Repo Version
  deriving (Eq, Show)

instance FromJSON PackageDescr where
  parseJSON (Object o) =
    PackageDescr <$> o .: "repo"
                 <*> o .: "version"
  parseJSON _ = mzero

-- | Type containing information about a package set
--   (as described in idr-package-set.json).
--   A package set is a collection of git repositories used for finding
--   projects (which contain packages).
newtype PackageSet = PackageSet (Map Text PackageDescr)
  deriving (Eq, Show)

instance FromJSON PackageSet where
  parseJSON v = PackageSet <$> parseJSON v

-- | Type alias for command line arguments passed to the run command.
type Argument = Text

-- | Type describing the various commands that can be passed in via the commandline interface.
data Command
  = Fetch                        -- ^ Fetches all dependencies as described in json file
  | Compile                      -- ^ Compiles all code (fetch needs to be done first to get dependencies code?)
  | Clean                        -- ^ Cleans up build artifacts, fetched code?
  | Run [Argument]               -- ^ Runs the executable defined in idr-project.json
  | Repl ProjectName PackageName  -- ^ Opens up the repl
  | New ProjectName              -- ^ Initializes a new project for use with idream
  | Add PackageName PackageType  -- ^ Adds a package to an existing idream project
  | MkDoc                        -- ^ Generates the documentation
  | GenerateIpkg                 -- ^ Generates a ipkg file from the idream JSON files
  | Test                         -- ^ Runs unit tests for this project
  deriving (Eq, Show)

-- | Data structure representing the arguments passed in to the program.
data Args = Args
  { argsSeverity :: Severity
  , argsCommand :: Command
  } deriving (Eq, Show)
