module Idream.Types.External
  ( Project (..)
  , Package (..)
  , LocalRepoRef (..)
  , GitRepoRef (..)
  , RepoRef (..)
  , PackageOverride (..)
  , PackageRef (..)
  , ProjectRef (..)
  , PackageSet (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Idream.Prelude
import Idream.Types.Common (Codegen, GitCommit, GitUrl, PackageName, PackageType, ProjectName, RepoName)

-- | Type containing project data (coming from idr-project.json).
--   A project consists of 1 or more packages that are closely related to
--   each other. This can for example be a binary, library, tests, ...
data Project = Project
  { projectName :: !ProjectName
  , projectCodegen :: !(Maybe Codegen)
  , projectPaths :: !(Maybe [FilePath])
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord Project)

-- | Type containing package data (coming from idr-package.json).
--   A package can depend on 1 or more projects (which can possibly contain
--   multiple closely related binaries or libraries).
data Package = Package
  { packageName :: !PackageName
  , packageType :: !(Maybe PackageType)
  , packageSourcedir :: !(Maybe Directory)
  , packageDepends :: !(Maybe [PackageName])
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord Package)

newtype LocalRepoRef = LocalRepoRef
  { lrrPath :: Directory
  } deriving newtype (Eq, Show, ToJSON, FromJSON)
    deriving stock (Generic)

data GitRepoRef = GitRepoRef
  { grrUrl :: !GitUrl
  , grrCommit :: !GitCommit
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord GitRepoRef)

data RepoRef =
    RepoRefLocal !LocalRepoRef
  | RepoRefGit !GitRepoRef
  deriving stock (Eq, Show, Generic)

instance FromJSON RepoRef where
  parseJSON = withObject "RepoRef" $ \o -> do
    mloc <- o .:? "local"
    mgit <- o .:? "git"
    case (mloc, mgit) of
      (Just loc, Nothing) -> pure (RepoRefLocal loc)
      (Nothing, Just git) -> pure (RepoRefGit git)
      _ -> fail "Expected exactly one of local/git"

instance ToJSON RepoRef where
  toJSON ref =
    case ref of
      RepoRefLocal x -> object ["local" .= x]
      RepoRefGit x -> object ["git" .= x]

newtype PackageOverride = PackageOverride
  { poSourcedir :: Maybe Directory
  } deriving newtype (Eq, Show)
    deriving stock (Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord PackageOverride)

data PackageRef = PackageRef
  { pkgRefRepo :: !RepoName
  , pkgRefSubdir :: !(Maybe Directory)
  , pkgRefIpkg :: !(Maybe FilePath)
  , pkgRefOverride :: !(Maybe PackageOverride)
  , pkgRefDepends :: !(Maybe [PackageName])
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord PackageRef)

data ProjectRef = ProjectRef
  { projRefRepo :: !RepoName
  , projRefSubdir :: !(Maybe Directory)
  , projRefPackages :: ![PackageName]
  } deriving stock (Eq, Show, Generic)

instance FromJSON ProjectRef where
  parseJSON = withObject "ProjectRef" $ \o -> do
    repo <- o .: "repo"
    subdir <- o .:? "subdir"
    pkgs <- o .: "packages"
    pure (ProjectRef repo subdir pkgs)

instance ToJSON ProjectRef where
  toJSON (ProjectRef repo subdir pkgs) = object
    [ "repo" .= repo
    , "subdir" .= subdir
    , "packages" .= pkgs
    ]

-- | Type containing information about a package set
--   (as described in idr-package-set.json).
--   A package set is a collection of git repositories used for finding
--   projects (which contain packages).
--   If needed at all, this should exist in the project root.
data PackageSet = PackageSet
  { psRepos :: !(Maybe (Map RepoName RepoRef))
  , psPackages :: !(Maybe (Map PackageName PackageRef))
  , psProjects :: !(Maybe [ProjectRef])
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord PackageSet)
