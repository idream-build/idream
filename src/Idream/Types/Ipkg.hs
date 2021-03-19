module Idream.Types.Ipkg
  ( PackageVersion (..)
  , PackageVersionBounds (..)
  , unrestrictedPVB
  , exactPVB
  , matchExactPVB
  , containsPVB
  , ModuleName (..)
  , extractModuleName
  , ModuleLoc (..)
  , extractModuleLoc
  , PackageDepends (..)
  , PackageDesc (..)
  , minPackageDesc
  ) where

import Data.List (groupBy, stripPrefix)
import qualified Data.Text as T
import Idream.Prelude
import Idream.Types.Common (PackageName)
import System.FilePath (dropExtension)

newtype PackageVersion = PackageVersion
  { unPkgVersion :: [Int]
  } deriving newtype (Eq, Show, Ord, ToJSON, FromJSON)
    deriving stock (Generic)

instance ToText PackageVersion where
  toText = T.intercalate "." . fmap toText . unPkgVersion

data PackageVersionBounds = PackageVersionBounds
  { pvbLowerBound :: !(Maybe PackageVersion)
  , pvbLowerInclusive :: !Bool -- >= if true
  , pvbUpperBound :: !(Maybe PackageVersion)
  , pvbUpperInclusive :: !Bool -- <= if true
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord PackageVersionBounds)

instance ToText PackageVersionBounds where
  toText pvb@(PackageVersionBounds mpvl li mpvu ui) =
    let ml = fmap (\pvl -> (if li then ">= " else "> ") <> toText pvl) mpvl
        mu = fmap (\pvu -> (if ui then "<= " else "< ") <> toText pvu) mpvu
        mv = matchExactPVB pvb
    in case mv of
      Just v -> "== " <> toText v
      Nothing ->
        case (ml, mu) of
          (Nothing, Nothing) -> ""
          (Nothing, Just u) -> u
          (Just l, Nothing) -> l
          (Just l, Just u) -> l <> " && " <> u

unrestrictedPVB :: PackageVersionBounds
unrestrictedPVB = PackageVersionBounds Nothing True Nothing True

exactPVB :: PackageVersion -> PackageVersionBounds
exactPVB pv = PackageVersionBounds (Just pv) True (Just pv) True

matchExactPVB :: PackageVersionBounds -> Maybe PackageVersion
matchExactPVB (PackageVersionBounds mpvl li mpvu ui) =
  if li && ui && mpvl == mpvu
    then mpvl
    else Nothing

containsPVB :: PackageVersion -> PackageVersionBounds -> Bool
containsPVB pv (PackageVersionBounds mpvl li mpvu ui) = lowerOk && upperOk where
  lowerOk = maybe True (\pvl -> if li then pvl <= pv else pvl < pv) mpvl
  upperOk = maybe True (\pvu -> if ui then pvu >= pv else pvu > pv) mpvu

data PackageDepends = PackageDepends
  { pdepName :: !PackageName
  , pdepBounds :: !PackageVersionBounds
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via (AesonRecord PackageDepends)

instance ToText PackageDepends where
  toText (PackageDepends pn pvb) =
    let k = toText pn
        v = toText pvb
    in if T.null v then k else k <> " " <> v

newtype ModuleName = ModuleName
  { unModuleName :: [Text]
  } deriving newtype (Eq, Show, Ord, ToJSON, FromJSON)
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
    deriving (ToJSON, FromJSON) via (AesonRecord ModuleLoc)

extractModuleLoc :: FilePath -> ModuleLoc
extractModuleLoc path = ModuleLoc (extractModuleName path) path

data PackageDesc = PackageDesc
  { pdescName :: !PackageName  -- ^ Name associated with a package.
  , pdescVersion :: !PackageVersion -- ^ Version of the package
  , pdescAuthors :: !(Maybe Text) -- ^ Author information.
  , pdescMaintainers :: !(Maybe Text)  -- ^ Maintainer information.
  , pdescLicense :: !(Maybe Text) -- ^ Description of the licensing information.
  , pdescBrief   :: !(Maybe Text)  -- ^ Brief description of the package.
  , pdescReadme  :: !(Maybe Text)  -- ^ Location of the README file.
  , pdescHomepage :: !(Maybe Text) -- ^ Website associated with the package.
  , pdescSourceloc :: !(Maybe Text)  -- ^ Location of the source files.
  , pdescBugtracker :: !(Maybe Text) -- ^ Location of the project's bug tracker.
  , pdescDepends :: ![PackageDepends] -- ^ Packages to add to search path
  , pdescModules :: ![ModuleName] -- ^ Modules to install
  , pdescMain :: !(Maybe ModuleName) -- ^ Main file (i.e. file to load at REPL)
  , pdescExecutable :: !(Maybe Text) -- ^ Name of executable
  , pdescOptions :: !(Maybe Text)  -- ^ List of options to give the compiler.
  , pdescSourcedir :: !(Maybe Text)  -- ^ Source directory for Idris files
  , pdescBuilddir :: !(Maybe Text)
  , pdescOutputdir :: !(Maybe Text)
  , pdescPrebuild :: !(Maybe Text) -- ^ Script to run before building
  , pdescPostbuild :: !(Maybe Text) -- ^ Script to run after building
  , pdescPreinstall :: !(Maybe Text) -- ^ Script to run after building, before installing
  , pdescPostinstall :: !(Maybe Text) -- ^ Script to run after installing
  , pdescPreclean :: !(Maybe Text) -- ^ Script to run before cleaning
  , pdescPostclean :: !(Maybe Text) -- ^ Script to run after cleaning
  } deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AesonRecord PackageDesc

instance ToText PackageDesc where
  toText pd =
    let pn = pdescName pd
        modules = pdescModules pd
        depends = pdescDepends pd
        modsList = T.intercalate ", " (fmap toText modules)
        depsList = T.intercalate ", " (fmap toText depends)
    in T.unlines $ filter (not . T.null)
      [ "package " <> toText pn
      , maybe "" (\x -> "sourcedir = \"" <> x <> "\"") (pdescSourcedir pd)
      , maybe "" (\x -> "main = " <> toText x) (pdescMain pd)
      , maybe "" (\x -> "executable = \"" <> x <> "\"") (pdescExecutable pd)
      , if null modules then "" else "modules = " <> modsList
      , if null depends then "" else "depends = " <> depsList
      ]

minPackageDesc :: PackageName -> PackageVersion -> PackageDesc
minPackageDesc pn pv = PackageDesc
  { pdescName = pn
  , pdescVersion = pv
  , pdescAuthors = Nothing
  , pdescMaintainers = Nothing
  , pdescLicense = Nothing
  , pdescBrief = Nothing
  , pdescReadme = Nothing
  , pdescHomepage = Nothing
  , pdescSourceloc = Nothing
  , pdescBugtracker = Nothing
  , pdescDepends = []
  , pdescModules = []
  , pdescMain = Nothing
  , pdescExecutable = Nothing
  , pdescOptions = Nothing
  , pdescSourcedir = Nothing
  , pdescBuilddir = Nothing
  , pdescOutputdir = Nothing
  , pdescPrebuild = Nothing
  , pdescPostbuild = Nothing
  , pdescPreinstall = Nothing
  , pdescPostinstall = Nothing
  , pdescPreclean = Nothing
  , pdescPostclean = Nothing
  }
