module Idream.Types.Common
  ( PackageName (..)
  , PackageType (..)
  , ProjectName (..)
  , RepoName (..)
  ) where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, Value (..), withText)
import Data.String (IsString)
import Data.Text (Text)

newtype PackageName = PackageName { unPkgName :: Text }
  deriving newtype (Eq, Ord, Show, ToJSONKey, FromJSONKey, ToJSON, FromJSON, IsString)

newtype ProjectName = ProjectName { unProjName :: Text }
  deriving newtype (Eq, Ord, Show, ToJSONKey, FromJSONKey, ToJSON, FromJSON, IsString)

data PackageType = PkgTypeLibrary | PkgTypeExecutable | PkgTypeTest
  deriving (Eq, Show, Ord, Enum, Bounded)

instance FromJSON PackageType where
  parseJSON =
    withText "PackageType" $ \case
      "library" -> pure PkgTypeLibrary
      "executable" -> pure PkgTypeExecutable
      "test" -> pure PkgTypeTest
      _ -> fail "Expected one of: library/executable/test"

instance ToJSON PackageType where
  toJSON p =
    case p of
      PkgTypeLibrary -> String "library"
      PkgTypeExecutable -> String "executable"
      PkgTypeTest -> String "test"

newtype RepoName = RepoName { unRepoName :: Text }
  deriving newtype (Eq, Ord, Show, ToJSONKey, FromJSONKey, ToJSON, FromJSON, IsString)
