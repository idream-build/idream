module Idream.Types.Common
  ( PackageName
  , ProjectName
  , RepoName
  , Codegen
  , GitUrl
  , GitCommit
  , PackageType (..)
  , packageTypeFromText
  , PackageGroup (..)
  , packageGroupFromList
  , RefreshStrategy (..)
  , refreshStratFromText
  ) where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, Value (..), withText)
import qualified Data.Set as Set
import Idream.Prelude

newtype PackageName = PackageName Text
  deriving newtype (Eq, Ord, Show, ToJSONKey, FromJSONKey, ToJSON, FromJSON,
                    IsString, ToString, IsText, ToText)

newtype ProjectName = ProjectName Text
  deriving newtype (Eq, Ord, Show, ToJSONKey, FromJSONKey, ToJSON, FromJSON,
                    IsString, ToString, IsText, ToText)

newtype RepoName = RepoName Text
  deriving newtype (Eq, Ord, Show, ToJSONKey, FromJSONKey, ToJSON, FromJSON,
                    IsString, ToString, IsText, ToText)

newtype Codegen = Codegen Text
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON,
                    IsString, ToString, IsText, ToText)

newtype GitUrl = GitUrl Text
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON,
                    IsString, ToString, IsText, ToText)

newtype GitCommit = GitCommit Text
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON,
                    IsString, ToString, IsText, ToText)

data PackageType = PkgTypeLibrary | PkgTypeExecutable | PkgTypeTest
  deriving (Eq, Show, Ord, Enum, Bounded)

packageTypeFromText :: Text -> Maybe PackageType
packageTypeFromText t =
  case t of
    "library" -> Just PkgTypeLibrary
    "executable" -> Just PkgTypeExecutable
    "test" -> Just PkgTypeTest
    _ -> Nothing

instance ToText PackageType where
  toText p =
    case p of
      PkgTypeLibrary -> "library"
      PkgTypeExecutable -> "executable"
      PkgTypeTest -> "test"

instance FromJSON PackageType where
  parseJSON =
    withText "PackageType" $
      maybe (fail "Expected one of: library/executable/test") pure . packageTypeFromText

instance ToJSON PackageType where
  toJSON = String . toText

-- | Package selector - all or a subset of packages
data PackageGroup =
    PackageGroupAll
  | PackageGroupSubset (Set PackageName)
  deriving (Eq, Show)

packageGroupFromList :: [Text] -> PackageGroup
packageGroupFromList ss =
  case ss of
    [] -> PackageGroupAll
    _ -> PackageGroupSubset (Set.fromList (fmap fromText ss))

-- | Whether to use the network to refresh repos
data RefreshStrategy = ForceRefresh | EnableRefresh | DisableRefresh
  deriving (Eq, Show, Enum, Bounded)

refreshStratFromText :: Text -> Maybe RefreshStrategy
refreshStratFromText t =
  case t of
    "force" -> Just ForceRefresh
    "enable" -> Just EnableRefresh
    "disable" -> Just DisableRefresh
    _ -> Nothing

instance ToText RefreshStrategy where
  toText r =
    case r of
      ForceRefresh -> "force"
      EnableRefresh -> "enable"
      DisableRefresh -> "disable"
