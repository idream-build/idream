module Idream.Types.External
  ( Project (..)
  , Package (..)
  , BuildType (..)
  , IpkgBuildSpec (..)
  , LocalRepoRef (..)
  , GitRepoRef (..)
  , RepoRef (..)
  , PackageRef (..)
  , PackageSet (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Map (Map)
import Data.Text (Text)
import Idream.FilePaths (Directory)
import Idream.Types.Common (PackageName, PackageType, ProjectName, RepoName)

-- | Type containing project data (coming from idr-project.json).
--   A project consists of 1 or more packages that are closely related to
--   each other. This can for example be a binary, library, tests, ...
data Project = Project
  { projectName :: ProjectName
  , projectPaths :: Maybe [FilePath]
  } deriving (Eq, Show)

instance FromJSON Project where
  parseJSON = withObject "Project" $ \o -> do
    name <- o .: "name"
    paths <- o .:? "paths"
    pure (Project name paths)

instance ToJSON Project where
  toJSON (Project name paths) =
    object ["name" .= toJSON name, "paths" .= toJSON paths]

-- | Type containing package data (coming from idr-package.json).
--   A package can depend on 1 or more projects (which can possibly contain
--   multiple closely related binaries or libraries).
data Package = Package
  { packageName :: PackageName
  , packageType :: Maybe PackageType
  , packageSourcedir :: Maybe Directory
  , packageDepends :: Maybe [PackageName]
  } deriving (Eq, Show)

instance FromJSON Package where
  parseJSON = withObject "Package" $ \o -> do
    name <- o .: "name"
    ty <- o .:? "type"
    sourcedir <- o .:? "sourcedir"
    depends <- o .:? "depends"
    pure (Package name ty sourcedir depends)

instance ToJSON Package where
  toJSON (Package name ty sourcedir depends) = object
    [ "name" .= name
    , "type" .= ty
    , "sourcedir" .= sourcedir
    , "depends" .= depends
    ]

data BuildType = BuildTypeIdream | BuildTypeIpkg deriving (Eq, Show, Ord, Enum, Bounded)

instance FromJSON BuildType where
  parseJSON =
    withText "BuildType" $ \case
      "idream" -> pure BuildTypeIdream
      "ipkg" -> pure BuildTypeIpkg
      _ -> fail "Expected one of: idream/ipkg"

instance ToJSON BuildType where
  toJSON b =
    case b of
      BuildTypeIdream -> String "idream"
      BuildTypeIpkg -> String "ipkg"

data IpkgBuildSpec = IpkgBuildSpec
  { ibsSourcedir :: Maybe Directory
  , ibsDepends :: Maybe [PackageName]
  } deriving (Eq, Show)

instance FromJSON IpkgBuildSpec where
  parseJSON = withObject "IpkgBuildSpec" $ \o -> do
    sourcedir <- o .:? "sourcedir"
    depends <- o .:? "depends"
    pure (IpkgBuildSpec sourcedir depends)

instance ToJSON IpkgBuildSpec where
  toJSON (IpkgBuildSpec sourcedir depends) =
    object ["sourcedir" .= toJSON sourcedir, "depends" .= toJSON depends]

newtype LocalRepoRef = LocalRepoRef
  { lrrPath :: Directory
  } deriving newtype (Eq, Show, ToJSON, FromJSON)

data GitRepoRef = GitRepoRef
  { grrUrl :: Text
  , grrCommit :: Text
  } deriving (Eq, Show)

instance FromJSON GitRepoRef where
  parseJSON = withObject "GitRepoRef" $ \o -> do
    url <- o .: "url"
    commit <- o .: "commit"
    pure (GitRepoRef url commit)

instance ToJSON GitRepoRef where
  toJSON (GitRepoRef url commit) =
    object ["url" .= toJSON url, "commit" .= toJSON commit]

data RepoRef =
    RepoRefLocal LocalRepoRef
  | RepoRefGit GitRepoRef
  deriving (Eq, Show)

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
      RepoRefLocal x -> object ["local" .= toJSON x]
      RepoRefGit x -> object ["git" .= toJSON x]

data PackageRef = PackageRef
  { prRepo :: RepoName
  , prType :: Maybe BuildType
  , prSpec :: Maybe IpkgBuildSpec
  , prSubdir :: Maybe Directory
  } deriving (Eq, Show)

instance FromJSON PackageRef where
  parseJSON = withObject "PackageRef" $ \o -> do
    repo <- o .: "repo"
    ty <- o .:? "type"
    spec <- o .:? "spec"
    subdir <- o .:? "subdir"
    pure (PackageRef repo ty spec subdir)

instance ToJSON PackageRef where
  toJSON (PackageRef repo ty spec subdir) =
    object ["repo" .= toJSON repo, "ty" .= toJSON ty, "spec" .= toJSON spec, "subdir" .= toJSON subdir]

-- | Type containing information about a package set
--   (as described in idr-package-set.json).
--   A package set is a collection of git repositories used for finding
--   projects (which contain packages).
--   If needed at all, this should exist in the project root.
data PackageSet = PackageSet
  { psRepos :: Maybe (Map RepoName RepoRef)
  , psPkgs :: Maybe (Map PackageName PackageRef)
  } deriving (Eq, Show)

instance FromJSON PackageSet where
  parseJSON = withObject "PackageSet" $ \o -> do
    repos <- o .:? "repos"
    pkgs <- o .:? "packages"
    pure (PackageSet repos pkgs)

instance ToJSON PackageSet where
  toJSON (PackageSet repos pkgs) =
    object ["repos" .= toJSON repos, "packages" .= toJSON pkgs]
