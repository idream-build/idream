module Idream.Types.Common
  ( PackageName (..)
  , PackageType (..)
  , packageTypeFromString
  , ProjectName (..)
  , RepoName (..)
  , PackageGroup (..)
  , packageGroupFromList
  , RefreshStrategy (..)
  , refreshStratFromString
  ) where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, Value (..), withText)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T

newtype PackageName = PackageName { unPkgName :: Text }
  deriving newtype (Eq, Ord, Show, ToJSONKey, FromJSONKey, ToJSON, FromJSON, IsString)

newtype ProjectName = ProjectName { unProjName :: Text }
  deriving newtype (Eq, Ord, Show, ToJSONKey, FromJSONKey, ToJSON, FromJSON, IsString)

data PackageType = PkgTypeLibrary | PkgTypeExecutable | PkgTypeTest
  deriving (Eq, Show, Ord, Enum, Bounded)

packageTypeFromString :: String -> Maybe PackageType
packageTypeFromString t =
  case t of
    "library" -> Just PkgTypeLibrary
    "executable" -> Just PkgTypeExecutable
    "test" -> Just PkgTypeTest
    _ -> Nothing

instance FromJSON PackageType where
  parseJSON =
    withText "PackageType" $ \t ->
      maybe (fail "Expected one of: library/executable/test") pure (packageTypeFromString (T.unpack t))

instance ToJSON PackageType where
  toJSON p =
    case p of
      PkgTypeLibrary -> String "library"
      PkgTypeExecutable -> String "executable"
      PkgTypeTest -> String "test"

newtype RepoName = RepoName { unRepoName :: Text }
  deriving newtype (Eq, Ord, Show, ToJSONKey, FromJSONKey, ToJSON, FromJSON, IsString)

-- | Package selector - all or a subset of packages
data PackageGroup =
    PackageGroupAll
  | PackageGroupSubset (Set PackageName)
  deriving (Eq, Show)

packageGroupFromList :: [String] -> PackageGroup
packageGroupFromList ss =
  case ss of
    [] -> PackageGroupAll
    _ -> PackageGroupSubset (Set.fromList (fmap (PackageName . T.pack) ss))

-- | Whether to use the network to refresh repos
data RefreshStrategy = ForceRefresh | EnableRefresh | DisableRefresh
  deriving (Eq, Show, Enum, Bounded)

refreshStratFromString :: String -> Maybe RefreshStrategy
refreshStratFromString t =
  case t of
    "force" -> Just ForceRefresh
    "enable" -> Just EnableRefresh
    "disable" -> Just DisableRefresh
    _ -> Nothing
