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

newtype BuiltinDepInfo = BuiltinDepInfo
  { builtinDepDepends :: [PackageName]
  } deriving newtype (Eq, Show)
    deriving stock (Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord BuiltinDepInfo)

data IdreamDepInfo = IdreamDepInfo
  { idreamDepLocal :: !Bool  -- local == is part of current project, not a ref
  , idreamDepPath :: !Directory
  , idreamDepType :: !PackageType
  , idreamDepSourcedir :: !(Maybe Directory)
  , idreamDepDepends :: ![PackageName]
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord IdreamDepInfo)

data IpkgDepInfo = IpkgDepInfo
  { ipkgDepPath :: !Directory
  , ipkgDepPkgfile :: !FilePath  -- relative to path
  , ipkgDepDepends :: ![PackageName]
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
    DepInfoIdream x -> idreamDepDepends x
    DepInfoIpkg x -> ipkgDepDepends x

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
