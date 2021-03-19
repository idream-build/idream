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
  deriving stock (Generic)

newtype ProjectName = ProjectName Text
  deriving newtype (Eq, Ord, Show, ToJSONKey, FromJSONKey, ToJSON, FromJSON,
                    IsString, ToString, IsText, ToText)
  deriving stock (Generic)

newtype RepoName = RepoName Text
  deriving newtype (Eq, Ord, Show, ToJSONKey, FromJSONKey, ToJSON, FromJSON,
                    IsString, ToString, IsText, ToText)
  deriving stock (Generic)

newtype Codegen = Codegen Text
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON,
                    IsString, ToString, IsText, ToText)
  deriving stock (Generic)

newtype GitUrl = GitUrl Text
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON,
                    IsString, ToString, IsText, ToText)
  deriving stock (Generic)

newtype GitCommit = GitCommit Text
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON,
                    IsString, ToString, IsText, ToText)
  deriving stock (Generic)

data PackageType = PkgTypeLibrary | PkgTypeExecutable | PkgTypeTest
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

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
  | PackageGroupSubset !(Set PackageName)
  deriving stock (Eq, Show, Generic)

packageGroupFromList :: [Text] -> PackageGroup
packageGroupFromList ss =
  case ss of
    [] -> PackageGroupAll
    _ -> PackageGroupSubset (Set.fromList (fmap fromText ss))

-- | Whether to use the network to refresh repos
data RefreshStrategy = ForceRefresh | EnableRefresh | DisableRefresh
  deriving stock (Eq, Show, Enum, Bounded, Generic)

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
