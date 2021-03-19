module Idream.Types.Ipkg
  ( PackageVersion (..)
  , PackageVersionBounds (..)
  , unrestrictedPVB
  , exactPVB
  , containsPVB
  , ModuleName (..)
  , extractModuleName
  , ModuleLoc (..)
  , extractModuleLoc
  , PackageDepends (..)
  , PackageDesc (..)
  ) where

import Data.List (groupBy, stripPrefix)
import qualified Data.Text as T
import Idream.Prelude
import Idream.Types.Common (PackageName)
import System.FilePath (dropExtension)

newtype PackageVersion = PackageVersion
  { unPkgVersion :: [Int]
  } deriving newtype (Eq, Show, Ord)
    deriving stock (Generic)

instance ToText PackageVersion where
  toText = T.intercalate "." . fmap toText . unPkgVersion

data PackageVersionBounds = PackageVersionBounds
  { pvbLowerBound :: !(Maybe PackageVersion)
  , pvbLowerInclusive :: !Bool -- >= if true
  , pvbUpperBound :: !(Maybe PackageVersion)
  , pvbUpperInclusive :: !Bool -- <= if true
  } deriving stock (Eq, Show, Generic)

unrestrictedPVB :: PackageVersionBounds
unrestrictedPVB = PackageVersionBounds Nothing True Nothing True

exactPVB :: PackageVersion -> PackageVersionBounds
exactPVB pv = PackageVersionBounds (Just pv) True (Just pv) True

containsPVB :: PackageVersion -> PackageVersionBounds -> Bool
containsPVB pv (PackageVersionBounds mpvl li mpvu ui) = lowerOk && upperOk where
  lowerOk = maybe True (\pvl -> if li then pvl <= pv else pvl < pv) mpvl
  upperOk = maybe True (\pvu -> if ui then pvu >= pv else pvu > pv) mpvu

data PackageDepends = PackageDepends
  { pdepName :: !PackageName
  , pdepBounds :: !PackageVersionBounds
  } deriving stock (Eq, Show, Generic)

newtype ModuleName = ModuleName
  { unModuleName :: [Text]
  } deriving newtype (Eq, Show, Ord)
    deriving stock (Generic)

instance ToText ModuleName where
  toText = T.intercalate "." . unModuleName

-- | Extracts the filename for use in ipkg file.
--   e.g. LightYear/Position.idr -> LightYear.Position
extractModuleName :: FilePath -> ModuleName
extractModuleName =
    ModuleName .
    fmap (toText . fmap replaceSlash . trimSlashPrefix) .
    splitParts .
    trimSlashPrefix .
    trimDotPrefix .
    dropExtension where
  splitParts = groupBy (\_ b -> b /= '/')
  replaceSlash '/' = '.'
  replaceSlash c = c
  trimDotPrefix s = fromMaybe s (stripPrefix "." s)
  trimSlashPrefix s = fromMaybe s (stripPrefix "/" s)

data ModuleLoc = ModuleLoc
  { mlName :: !ModuleName
  , mlPath :: !FilePath
  } deriving stock (Eq, Show, Generic)

extractModuleLoc :: FilePath -> ModuleLoc
extractModuleLoc path = ModuleLoc (extractModuleName path) path

data PackageDesc = PackageDesc
  { pdescName :: !PackageName  -- ^ Name associated with a package.
  , pdescVersion :: !PackageVersion -- ^ Version of the package
  , pdescAuthors :: !String -- ^ Author information.
  , pdescMaintainers :: !(Maybe String)  -- ^ Maintainer information.
  , pdescLicense :: !(Maybe String) -- ^ Description of the licensing information.
  , pdescBrief   :: !(Maybe String)  -- ^ Brief description of the package.
  , pdescReadme  :: !(Maybe String)  -- ^ Location of the README file.
  , pdescHomepage :: !(Maybe String) -- ^ Website associated with the package.
  , pdescSourceloc :: !(Maybe String)  -- ^ Location of the source files.
  , pdescBugtracker :: !(Maybe String) -- ^ Location of the project's bug tracker.
  , pdescDepends :: ![PackageDepends] -- ^ Packages to add to search path
  , pdescModules :: ![ModuleName] -- ^ Modules to install
  , pdescMainmod :: ![ModuleName] -- ^ Main file (i.e. file to load at REPL)
  , pdescExecutable :: !(Maybe String) -- ^ Name of executable
  , pdescOptions :: !(Maybe String)  -- ^ List of options to give the compiler.
  , pdescSourcedir :: !(Maybe String)  -- ^ Source directory for Idris files
  , pdescBuilddir :: !(Maybe String)
  , pdescOutputdir :: !(Maybe String)
  , pdescPrebuild :: !(Maybe String) -- ^ Script to run before building
  , pdescPostbuild :: !(Maybe String) -- ^ Script to run after building
  , pdescPreinstall :: !(Maybe String) -- ^ Script to run after building, before installing
  , pdescPostinstall :: !(Maybe String) -- ^ Script to run after installing
  , pdescPreclean :: !(Maybe String) -- ^ Script to run before cleaning
  , pdescPostclean :: !(Maybe String) -- ^ Script to run after cleaning
  } deriving stock (Eq, Show, Generic)
