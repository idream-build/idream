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

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:?), (.=))
import Idream.Prelude
import Idream.Types.Common (Codegen, GitCommit, GitUrl, PackageName, PackageType, ProjectName, RepoName)
import Idream.Types.Ipkg (PackageVersion)

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
  , packageVersion :: !(Maybe PackageVersion)
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

data PackageOverride = PackageOverride
  { poVersion :: !(Maybe PackageVersion)
  , poSourcedir :: !(Maybe Directory)
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord PackageOverride)

data PackageRef = PackageRef
  { pkrRepo :: !RepoName
  , pkrSubdir :: !(Maybe Directory)
  , pkrIpkg :: !(Maybe FilePath)
  , pkrOverride :: !(Maybe PackageOverride)
  , pkrDepends :: !(Maybe [PackageName])
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord PackageRef)

data ProjectRef = ProjectRef
  { pjrRepo :: !RepoName
  , pjrSubdir :: !(Maybe Directory)
  , pjrPackages :: ![PackageName]
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord ProjectRef)

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
