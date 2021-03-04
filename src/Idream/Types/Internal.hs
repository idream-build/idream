module Idream.Types.Internal
  ( BuiltinDepInfo (..)
  , IdreamDepInfo (..)
  , IpkgDepInfo (..)
  , DepInfo (..)
  , depInfoDepends
  , DepInfoMap (..)
  , LocatedPackage (..)
  , ResolvedProject (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Idream.Prelude
import Idream.Types.Common (PackageName, PackageType, ProjectName)
import Idream.Types.External (Package)

newtype BuiltinDepInfo = BuiltinDepInfo { builtinDepDepends :: [PackageName] }
  deriving (Eq, Show)

instance FromJSON BuiltinDepInfo where
  parseJSON = withObject "BuiltinDepInfo" $ \o ->
    fmap BuiltinDepInfo (o .: "depends")

instance ToJSON BuiltinDepInfo where
  toJSON (BuiltinDepInfo depends) =
    object ["depends" .= toJSON depends]

data IdreamDepInfo = IdreamDepInfo
  { idreamDepLocal :: Bool  -- local == is part of current project, not a ref
  , idreamDepPath :: Directory
  , idreamDepType :: PackageType
  , idreamDepSourcedir :: Maybe Directory
  , idreamDepDepends :: [PackageName]
  } deriving (Eq, Show)

instance FromJSON IdreamDepInfo where
  parseJSON = withObject "IdreamDepInfo" $ \o -> do
    local <- o .: "local"
    path <- o .: "path"
    ty <- o .: "type"
    sourcedir <- o .:? "sourcedir"
    depends <- o .: "depends"
    pure (IdreamDepInfo local path ty sourcedir depends)

instance ToJSON IdreamDepInfo where
  toJSON (IdreamDepInfo local path ty sourcedir depends) = object
    [ "local" .= toJSON local
    , "path" .= toJSON path
    , "type" .= toJSON ty
    , "sourcedir" .= toJSON sourcedir
    , "depends" .= toJSON depends
    ]

data IpkgDepInfo = IpkgDepInfo
  { ipkgDepPath :: Directory
  , ipkgDepPkgFile :: FilePath  -- relative to path
  , ipkgDepDepends :: [PackageName]
  } deriving (Eq, Show)

instance FromJSON IpkgDepInfo where
  parseJSON = withObject "IpkgDepInfo" $ \o -> do
    path <- o .: "path"
    pkgFile <- o .: "pkgfile"
    depends <- o .: "depends"
    pure (IpkgDepInfo path pkgFile depends)

instance ToJSON IpkgDepInfo where
  toJSON (IpkgDepInfo path pkgFile depends) = object
    [ "path" .= toJSON path
    , "pkgfile" .= toJSON pkgFile
    , "depends" .= toJSON depends
    ]

data DepInfo =
    DepInfoBuiltin BuiltinDepInfo
  | DepInfoIdream IdreamDepInfo
  | DepInfoIpkg IpkgDepInfo
  deriving (Eq, Show)

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
      DepInfoBuiltin x -> object ["builtin" .= toJSON x]
      DepInfoIdream x -> object ["idream" .= toJSON x]
      DepInfoIpkg x -> object ["ipkg" .= toJSON x]

depInfoDepends :: DepInfo -> [PackageName]
depInfoDepends d =
  case d of
    DepInfoBuiltin x -> builtinDepDepends x
    DepInfoIdream x -> idreamDepDepends x
    DepInfoIpkg x -> ipkgDepDepends x

newtype DepInfoMap = DepInfoMap { unDepInfoMap :: Map PackageName DepInfo }
  deriving newtype (Eq, Show, ToJSON)

data LocatedPackage = LocatedPackage
  { lpPath :: Directory
  , lpPkg :: Package
  } deriving (Eq, Show)

data ResolvedProject = ResolvedProject
  { rpName :: ProjectName
  , rpPackages :: Map PackageName LocatedPackage
  } deriving (Eq, Show)
