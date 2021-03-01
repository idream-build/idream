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
  , withResolvedProject
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
import Idream.Deps (Deps (..), closureDeps, composeDeps, depsFromEdges, depsFromGroups, filterDeps, unionAllDeps, unionDeps)
import Idream.Effects.Serde (serdeReadJSON)
import Idream.FileLogic (pkgFileName, projFileName)
import Idream.FilePaths (Directory)
import Idream.Types.Common (PackageName (..), RepoName (..))
import Idream.Types.External (Package (..), PackageRef (..), PackageSet (..), Project (..), RepoRef (..))
import Idream.Types.Internal (ResolvedProject (..))
import System.FilePath ((</>))
import LittleLogger (logWarning)

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
    PackageGroupSubset s -> fst (filterDeps (`Set.member` s) ipd)

withResolvedProject :: Text -> Directory -> (ResolvedProject -> AppM ()) -> AppM ()
withResolvedProject step projDir act = do
  proj <- readProjFile (projDir </> projFileName)
  rp <- resolveProj proj
  if null (rpPackages rp)
    then logWarning ("Project contains no packages yet, skipping " <> step <> " step."
                    <> "Use `idream add` to add a package to this project first.")
    else act rp
