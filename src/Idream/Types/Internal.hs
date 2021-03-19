module Idream.Types.Internal
  ( BuiltinDepInfo (..)
  , IdreamDepInfo (..)
  , IpkgDepInfo (..)
  , DepInfo (..)
  , depInfoDepends
  , DepInfoMap
  , LocatedPackage (..)
  , ResolvedProject (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:?), (.=))
import Idream.Prelude
import Idream.Types.Common (Codegen, PackageName, PackageType, ProjectName)
import Idream.Types.External (Package)
import Idream.Types.Ipkg (PackageVersion)

newtype BuiltinDepInfo = BuiltinDepInfo
  { builtinDepDepends :: [PackageName]
  } deriving newtype (Eq, Show)
    deriving stock (Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord BuiltinDepInfo)

data IdreamDepInfo = IdreamDepInfo
  { iddLocal :: !Bool  -- local == is part of current project, not a ref
  , iddVersion :: !(Maybe PackageVersion)
  , iddPath :: !Directory
  , iddType :: !PackageType
  , iddSourcedir :: !(Maybe Directory)
  , iddDepends :: ![PackageName]
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord IdreamDepInfo)

data IpkgDepInfo = IpkgDepInfo
  { ipdPath :: !Directory
  , ipdPkgfile :: !FilePath  -- relative to path
  , ipdDepends :: ![PackageName]
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord IpkgDepInfo)

data DepInfo =
    DepInfoBuiltin !BuiltinDepInfo
  | DepInfoIdream !IdreamDepInfo
  | DepInfoIpkg !IpkgDepInfo
  deriving stock (Eq, Show, Generic)

instance FromJSON DepInfo where
  parseJSON = withObject "DepInfo" $ \o -> do
    mbuiltin <- o .:? "builtin"
    midream <- o .:? "idream"
    mipkg <- o .:? "ipkg"
    case (mbuiltin, midream, mipkg) of
      (Just x, Nothing, Nothing) ->
        pure (DepInfoBuiltin x)
      (Nothing, Just x, Nothing) ->
        pure (DepInfoIdream x)
      (Nothing, Nothing, Just x) ->
        pure (DepInfoIpkg x)
      _ -> fail "Expected exactly one of builtin/idream/ipkg"

instance ToJSON DepInfo where
  toJSON d =
    case d of
      DepInfoBuiltin x -> object ["builtin" .= x]
      DepInfoIdream x -> object ["idream" .= x]
      DepInfoIpkg x -> object ["ipkg" .= x]

depInfoDepends :: DepInfo -> [PackageName]
depInfoDepends d =
  case d of
    DepInfoBuiltin x -> builtinDepDepends x
    DepInfoIdream x -> iddDepends x
    DepInfoIpkg x -> ipdDepends x

type DepInfoMap = Map PackageName DepInfo

data LocatedPackage = LocatedPackage
  { lpPath :: !Directory
  , lpPackage :: !Package
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord LocatedPackage)

data ResolvedProject = ResolvedProject
  { rpName :: !ProjectName
  , rpCodegen :: !Codegen
  , rpPackages :: !(Map PackageName LocatedPackage)
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord ResolvedProject)
