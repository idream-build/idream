module Idream.Types.External
  ( Project (..)
  , Package (..)
  , LocalRepoRef (..)
  , GitRepoRef (..)
  , RepoRef (..)
  , PackageRef (..)
  , ProjectRef (..)
  , PackageSet (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText, (.:), (.:?), (.=))
import Idream.Prelude
import Idream.Types.Common (Codegen, GitUrl, GitCommit, PackageName, PackageType, ProjectName, RepoName)

-- | Type containing project data (coming from idr-project.json).
--   A project consists of 1 or more packages that are closely related to
--   each other. This can for example be a binary, library, tests, ...
data Project = Project
  { projectName :: ProjectName
  , projectCodegen :: Maybe Codegen
  , projectPaths :: Maybe [FilePath]
  } deriving (Eq, Show)

instance FromJSON Project where
  parseJSON = withObject "Project" $ \o -> do
    name <- o .: "name"
    codegen <- o .:? "codegen"
    paths <- o .:? "paths"
    pure (Project name codegen paths)

instance ToJSON Project where
  toJSON (Project name codegen paths) = object
    [ "name" .= name
    , "codegen" .= codegen
    , "paths" .= paths
    ]

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

newtype LocalRepoRef = LocalRepoRef
  { lrrPath :: Directory
  } deriving newtype (Eq, Show, ToJSON, FromJSON)

data GitRepoRef = GitRepoRef
  { grrUrl :: GitUrl
  , grrCommit :: GitCommit
  } deriving (Eq, Show)

instance FromJSON GitRepoRef where
  parseJSON = withObject "GitRepoRef" $ \o -> do
    url <- o .: "url"
    commit <- o .: "commit"
    pure (GitRepoRef url commit)

instance ToJSON GitRepoRef where
  toJSON (GitRepoRef url commit) =
    object ["url" .= url, "commit" .= commit]

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
      RepoRefLocal x -> object ["local" .= x]
      RepoRefGit x -> object ["git" .= x]

data PackageRef = PackageRef
  { pkgRefRepo :: RepoName
  , pkgRefSubdir :: Maybe Directory
  , pkgRefDepends :: Maybe [PackageName]
  } deriving (Eq, Show)

instance FromJSON PackageRef where
  parseJSON = withObject "PackageRef" $ \o -> do
    repo <- o .: "repo"
    subdir <- o .:? "subdir"
    depends <- o .:? "depends"
    pure (PackageRef repo subdir depends)

instance ToJSON PackageRef where
  toJSON (PackageRef repo subdir depends) = object
    [ "repo" .= repo
    , "subdir" .= subdir
    , "depends" .= depends
    ]

data ProjectRef = ProjectRef
  { projRefRepo :: RepoName
  , projRefSubdir :: Maybe Directory
  , projRefPkgs :: [PackageName]
  } deriving (Eq, Show)

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
  { psRepos :: Maybe (Map RepoName RepoRef)
  , psPkgs :: Maybe (Map PackageName PackageRef)
  , psProjects :: Maybe [ProjectRef]
  } deriving (Eq, Show)

instance FromJSON PackageSet where
  parseJSON = withObject "PackageSet" $ \o -> do
    repos <- o .:? "repos"
    pkgs <- o .:? "packages"
    projects <- o .:? "projects"
    pure (PackageSet repos pkgs projects)

instance ToJSON PackageSet where
  toJSON (PackageSet repos pkgs projects) = object
    [ "repos" .= repos
    , "packages" .= pkgs
    , "projects" .= projects
    ]
