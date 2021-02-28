module Idream.Types.Internal
  ( BuiltinDepInfo (..)
  , ProjectDepInfo (..)
  , RefDepInfo (..)
  , DepInfo (..)
  , DepInfoMap (..)
  , DepGraphMap (..)
  , ResolvedProject (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Map (Map)
import Data.Set (Set)
import Idream.FilePaths (Directory)
import Idream.Types.Common (PackageName, PackageType, ProjectName, RepoName)
import Idream.Types.External (BuildType, Package)

newtype BuiltinDepInfo = BuiltinDepInfo { bdiDepends :: [PackageName] }
  deriving (Eq, Show)

instance FromJSON BuiltinDepInfo where
  parseJSON = withObject "BuiltinDepInfo" $ \o ->
    fmap BuiltinDepInfo (o .: "depends")

instance ToJSON BuiltinDepInfo where
  toJSON (BuiltinDepInfo depends) =
    object ["depends" .= toJSON depends]

data ProjectDepInfo = ProjectDepInfo
  { pdiType :: PackageType
  , pdiSourcedir :: Maybe Directory
  , pdiDepends :: [PackageName]
  } deriving (Eq, Show)

instance FromJSON ProjectDepInfo where
  parseJSON = withObject "ProjectDepInfo" $ \o -> do
    ty <- o .: "type"
    sourcedir <- o .:? "sourcedir"
    depends <- o .: "depends"
    pure (ProjectDepInfo ty sourcedir depends)

instance ToJSON ProjectDepInfo where
  toJSON (ProjectDepInfo ty sourcedir depends) =
    object ["type" .= toJSON ty, "sourcedir" .= toJSON sourcedir, "depends" .= toJSON depends]

data RefDepInfo = RefDepInfo
  { rdiBuildType :: BuildType
  , rdiRepoName :: RepoName
  , rdiSubdir :: Maybe Directory
  , rdiSourcedir :: Maybe Directory
  , rdiDepends :: [PackageName]
  } deriving (Eq, Show)

instance FromJSON RefDepInfo where
  parseJSON = withObject "RefDepInfo" $ \o -> do
    ty <- o .: "type"
    repo <- o .: "repo"
    subdir <- o .:? "subdir"
    sourcedir <- o .:? "sourcedir"
    depends <- o .: "depends"
    pure (RefDepInfo ty repo subdir sourcedir depends)

instance ToJSON RefDepInfo where
  toJSON (RefDepInfo ty rn subdir sourcedir depends) = object
    [ "type" .= toJSON ty
    , "repo" .= toJSON rn
    , "subdir" .= toJSON subdir
    , "sourcedir" .= toJSON sourcedir
    , "depends" .= toJSON depends
    ]

data DepInfo =
    DepInfoBuiltin BuiltinDepInfo
  | DepInfoProject ProjectDepInfo
  | DepInfoRef RefDepInfo
  deriving (Eq, Show)

instance FromJSON DepInfo where
  parseJSON = withObject "DepInfo" $ \o -> do
    mbuiltin <- o .:? "builtin"
    mproject <- o .:? "project"
    mref <- o .:? "ref"
    case (mbuiltin, mproject, mref) of
      (Just x, Nothing, Nothing) ->
        pure (DepInfoBuiltin x)
      (Nothing, Just x, Nothing) ->
        pure (DepInfoProject x)
      (Nothing, Nothing, Just x) ->
        pure (DepInfoRef x)
      _ -> fail "Expected exactly one of builtin/project/ref"

instance ToJSON DepInfo where
  toJSON d =
    case d of
      DepInfoBuiltin x -> object ["builtin" .= toJSON x]
      DepInfoProject x -> object ["project" .= toJSON x]
      DepInfoRef x -> object ["ref" .= toJSON x]

newtype DepInfoMap = DepInfoMap { unDepInfoMap :: Map PackageName DepInfo }
  deriving newtype (Eq, Show, ToJSON)

newtype DepGraphMap = DepGraphMap { unDepGraphMap :: Map PackageName (Set PackageName) }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data ResolvedProject = ResolvedProject
  { rpName :: ProjectName
  , rpPackages :: [(Directory, Package)]
  } deriving (Eq, Show)
