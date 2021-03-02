module Idream.Command.Common
  ( readPkgFile
  , readPkgSetFile
  , readProjFile
  , resolveProj
  , PackageGroup (..)
  , initPkgDeps
  , initRepoDeps
  , repoDeps
  , allRepos
  , mkPkgGroup
  , pkgGroupMember
  , pkgGroupToText
  , reposForGroup
  , pkgDepsForGroup
  , fullPkgDepsForGroup
  , withResolvedProject
  , mkDepInfoMap
  , PkgParseErr (..)
  , PkgSetParseErr (..)
  , ProjParseErr (..)
  ) where

import Control.Exception (Exception (..))
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import Idream.App (AppM)
import Idream.Deps (Deps (..), closureDeps, composeDeps, depsFromEdges, depsFromGroups, restrictDeps, unionAllDeps,
                    unionDeps, depsVertices, depsFromMap)
import Idream.Effects.Serde (serdeReadJSON)
import Idream.FileLogic (pkgFileName, projFileName, fetchDir)
import Idream.FilePaths (Directory)
import Idream.Types.Common (PackageName (..), RepoName (..), PackageType (..))
import Idream.Types.External (Package (..), PackageRef (..), PackageSet (..), Project (..), RepoRef (..), BuildType (..), LocalRepoRef (..))
import Idream.Types.Internal (ResolvedProject (..), DepInfoMap (..), DepInfo (..), IdreamDepInfo (..), BuiltinDepInfo (..), IpkgDepInfo (..), depInfoDepends)
import LittleLogger (logWarning)
import System.FilePath ((</>), isExtensionOf, makeRelative, isPathSeparator)
import Idream.Effects.FileSystem (fsFindFiles)

-- | Error type for describing errors when parsing project file.
data ProjParseErr = ProjParseErr FilePath String
  deriving (Eq, Show)

instance Exception ProjParseErr where
  displayException (ProjParseErr path err) =
    "Failed to parse project file at " <> path <> ": " <> err <> "."

-- | Error type used for describing errors that can occur while reading out a package file.
data PkgParseErr = PkgParseErr FilePath String
  deriving (Eq, Show)

instance Exception PkgParseErr where
  displayException (PkgParseErr path err) =
    "Failed to parse package file at " <> path <> ": " <> err <> "."

-- | Error type used for describing errors that can occur while reading out a package set file.
data PkgSetParseErr = PkgSetParseErr FilePath String
  deriving (Eq, Show)

instance Exception PkgSetParseErr where
  displayException (PkgSetParseErr path err) =
    "Failed to parse package set file at " <> path <> ": " <> err <> "."

-- | Reads out a package file (idr-package.json)
readPkgFile :: FilePath -> AppM Package
readPkgFile path = serdeReadJSON (PkgParseErr path) path

-- | Reads out a package set file (idr-package-set.json)
readPkgSetFile :: FilePath -> AppM PackageSet
readPkgSetFile path = serdeReadJSON (PkgSetParseErr path) path

-- | Reads out a project file (idr-project.json).
readProjFile :: FilePath -> AppM Project
readProjFile path = serdeReadJSON (ProjParseErr path) path

-- | Reads project and package info into one struct.
resolveProj :: Project -> AppM ResolvedProject
resolveProj (Project pn mpaths) = do
  let paths = fromMaybe [] mpaths
  pairs <- for paths $ \path -> do
    pkg <- readPkgFile (path </> pkgFileName)
    pure (path, pkg)
  pure (ResolvedProject pn pairs)

initRepoDeps :: PackageSet -> Deps PackageName RepoName
initRepoDeps (PackageSet _ pkgs) = depsFromEdges edges where
  edges = fmap mkEdge (maybe [] Map.toList pkgs)
  mkEdge (name, PackageRef repo _ _ _) = (name, repo)

initPkgDeps :: ResolvedProject -> Deps PackageName PackageName
initPkgDeps (ResolvedProject  _ pkgs) = depsFromGroups groups where
  groups = fmap (mkGroup . snd) pkgs
  mkGroup (Package name _ _ depends) = (name, maybe Set.empty Set.fromList depends)

repoDeps :: ResolvedProject -> PackageSet -> Deps PackageName RepoName
repoDeps rp ps = composeDeps (closureDeps (initPkgDeps rp)) (initRepoDeps ps)

allRepos :: ResolvedProject -> PackageSet -> Set RepoName
allRepos rp ps = unionAllDeps (repoDeps rp ps)

specificRepos :: Foldable f => ResolvedProject -> PackageSet -> f PackageName -> Set RepoName
specificRepos rp ps pns = unionDeps pns (repoDeps rp ps)

data PackageGroup =
    PackageGroupAll
  | PackageGroupSubset (Set PackageName)
  deriving (Eq, Show)

mkPkgGroup :: Foldable f => f PackageName -> PackageGroup
mkPkgGroup pns =
  case toList pns of
    [] -> PackageGroupAll
    x -> PackageGroupSubset (Set.fromList x)

pkgGroupMember :: PackageName -> PackageGroup -> Bool
pkgGroupMember p g =
  case g of
    PackageGroupAll -> True
    PackageGroupSubset s -> Set.member p s

pkgGroupToText :: PackageGroup -> Text
pkgGroupToText g =
  case g of
    PackageGroupAll -> "all packages"
    PackageGroupSubset pns -> "selected packages (" <> T.intercalate ", " (fmap unPkgName (Set.toList pns)) <> ")"

reposForGroup :: ResolvedProject -> PackageSet -> PackageGroup -> Map RepoName RepoRef
reposForGroup rp ps g =
  let repos = case g of
        PackageGroupAll -> allRepos rp ps
        PackageGroupSubset pns -> specificRepos rp ps pns
      refs = fromMaybe Map.empty (psRepos ps)
  in Map.fromList (fmap (\r -> (r, refs Map.! r)) (Set.toList repos))

pkgDepsForGroup :: ResolvedProject -> PackageGroup -> Deps PackageName PackageName
pkgDepsForGroup rp g =
  let ipd = initPkgDeps rp
  in case g of
    PackageGroupAll -> ipd
    PackageGroupSubset s -> restrictDeps (`Set.member` s) ipd

depInfoPkgDeps :: DepInfoMap -> Deps PackageName PackageName
depInfoPkgDeps = depsFromGroups . fmap (fmap (Set.fromList . depInfoDepends)) . Map.toList . unDepInfoMap

fullPkgDepsForGroup :: ResolvedProject -> PackageGroup -> DepInfoMap -> Deps PackageName PackageName
fullPkgDepsForGroup rp g dim =
  let x = pkgDepsForGroup rp g
      y = depInfoPkgDeps dim
      z = depsVertices x
  in restrictDeps (`Set.member` z) y

withResolvedProject :: Text -> Directory -> (ResolvedProject -> AppM ()) -> AppM ()
withResolvedProject step projDir act = do
  proj <- readProjFile (projDir </> projFileName)
  rp <- resolveProj proj
  if null (rpPackages rp)
    then logWarning ("Project contains no packages yet, skipping " <> step <> " step."
                    <> "Use `idream add` to add a package to this project first.")
    else act rp

mkProjectDepInfo :: Directory -> Package -> IdreamDepInfo
mkProjectDepInfo path (Package _ mty msourcedir mdepends) = pdi where
  ty = fromMaybe PkgTypeLibrary mty
  depends = fromMaybe [] mdepends
  pdi = IdreamDepInfo True path ty msourcedir depends

mkProjectDepPair :: Directory -> Package -> (PackageName, DepInfo)
mkProjectDepPair path pkg = (packageName pkg, DepInfoIdream (mkProjectDepInfo path pkg))

mkBuiltinDepPairs :: [(PackageName, DepInfo)]
mkBuiltinDepPairs =
  fmap (fmap DepInfoBuiltin)
  [ ("base", BuiltinDepInfo [])
  , ("prelude", BuiltinDepInfo [])
  , ("contrib", BuiltinDepInfo [])
  , ("network", BuiltinDepInfo ["contrib"])
  ]

getRepoDir :: RepoName -> RepoRef -> Directory
getRepoDir rn rr =
  case rr of
    RepoRefLocal (LocalRepoRef d) -> d
    _ -> fetchDir </> T.unpack (unRepoName rn)

mkRefDepInfo :: Map RepoName RepoRef -> PackageRef -> AppM DepInfo
mkRefDepInfo repoRefs (PackageRef rn mbty msubdir mdepends) = do
  case Map.lookup rn repoRefs of
    Nothing -> error "TODO - throw for missing repo"
    Just rr -> do
      let repoDir = getRepoDir rn rr
          path = maybe repoDir (repoDir </>) msubdir
      case fromMaybe BuildTypeIpkg mbty of
        BuildTypeIpkg -> do
          let depends = fromMaybe [] mdepends
          pkgCand <- fmap (fmap (makeRelative path)) (fsFindFiles (isExtensionOf "ipkg") (Just path))
          case filter (not . any isPathSeparator) pkgCand of
            [longPkgFile] -> do
              let pkgFile = makeRelative path longPkgFile
              pure (DepInfoIpkg (IpkgDepInfo path pkgFile depends))
            _ -> error ("TODO - throw for no pkg " <> show path <> " " <> show pkgCand)
        BuildTypeIdream -> error "TODO - read idream dep depends"

mkRefDepPair :: Map RepoName RepoRef -> PackageName -> PackageRef -> AppM (PackageName, DepInfo)
mkRefDepPair repoRefs pn pr = fmap (pn,) (mkRefDepInfo repoRefs pr)

mkDepInfoMap :: ResolvedProject -> PackageSet -> AppM DepInfoMap
mkDepInfoMap rp ps = do
  let projectPairs = fmap (uncurry mkProjectDepPair) (rpPackages rp)
      builtinPairs = mkBuiltinDepPairs
      repoRefs = fromMaybe Map.empty (psRepos ps)
      pkgRefs = maybe [] Map.toList (psPkgs ps)
  refPairs <- for pkgRefs (uncurry (mkRefDepPair repoRefs))
  let m = Map.fromList (projectPairs ++ builtinPairs ++ refPairs)
  pure (DepInfoMap m)
