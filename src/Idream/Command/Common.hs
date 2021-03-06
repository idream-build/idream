module Idream.Command.Common
  ( findExtRel
  , readPkgFile
  , readPkgSetFile
  , readProjFile
  , mkProgramSpec
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
  , fullPkgDepsForGroup
  , readResolvedProject
  , readDepInfoMap
  , mkDepInfoMap
  , getDepInfoMap
  , PkgParseErr (..)
  , PkgSetParseErr (..)
  , ProjParseErr (..)
  , DuplicatePackageInSetErr (..)
  , DuplicatePackageInResolvedErr (..)
  , MissingRepoInPackageSetErr (..)
  , MissingDeclaredPackageErr (..)
  , NoUniqueIpkgErr (..)
  , UnsupportedCodegenExeErr (..)
  , MissingPackageInResolvedErr (..)
  , IpkgOverrideErr (..)
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Idream.Deps (Deps (..), closureDeps, composeDeps, depsFromEdges, depsFromGroups, restrictDeps, unionAllDeps,
                    unionDeps)
import Idream.Effects.FileSystem (fsFindFiles)
import Idream.Effects.Process (Spec (..))
import Idream.Effects.Serde (serdeReadJSON)
import Idream.FileLogic (outputDir, pkgFileName, pkgSetFileName, projFileName, repoFetchDir)
import Idream.Prelude
import Idream.Types.Common (Codegen, PackageGroup (..), PackageName, PackageType (..), ProjectName, RepoName)
import Idream.Types.External (LocalRepoRef (..), Package (..), PackageOverride (..), PackageRef (..), PackageSet (..),
                              Project (..), ProjectRef (..), RepoRef (..))
import Idream.Types.Internal (BuiltinDepInfo (..), DepInfo (..), DepInfoMap, IdreamDepInfo (..), IpkgDepInfo (..),
                              LocatedPackage (..), ResolvedProject (..), depInfoDepends)
import System.FilePath (isExtensionOf, isPathSeparator, makeRelative)

-- | Error type for describing errors when parsing project file.
data ProjParseErr = ProjParseErr FilePath String
  deriving (Eq, Show)

instance Exception ProjParseErr where
  displayException (ProjParseErr path err) =
    "Failed to parse project file at " <> path <> ": " <> err

-- | Error type used for describing errors that can occur while reading out a package file.
data PkgParseErr = PkgParseErr FilePath String
  deriving (Eq, Show)

instance Exception PkgParseErr where
  displayException (PkgParseErr path err) =
    "Failed to parse package file at " <> path <> ": " <> err

-- | Error type used for describing errors that can occur while reading out a package set file.
data PkgSetParseErr = PkgSetParseErr FilePath String
  deriving (Eq, Show)

instance Exception PkgSetParseErr where
  displayException (PkgSetParseErr path err) =
    "Failed to parse package set file at " <> path <> ": " <> err

newtype DuplicatePackageInSetErr = DuplicatePackageInSetErr PackageName
  deriving (Eq, Show)

instance Exception DuplicatePackageInSetErr where
  displayException (DuplicatePackageInSetErr pn) =
    "Duplicate package in initial package set: " <> toString pn

newtype DuplicatePackageInResolvedErr = DuplicatePackageInResolvedErr PackageName
  deriving (Eq, Show)

instance Exception DuplicatePackageInResolvedErr where
  displayException (DuplicatePackageInResolvedErr pn) =
    "Duplicate package in resolved package set: " <> toString pn

newtype MissingRepoInPackageSetErr = MissingRepoInPackageSetErr RepoName
  deriving (Eq, Show)

instance Exception MissingRepoInPackageSetErr where
  displayException (MissingRepoInPackageSetErr rn) =
    "Missing repo definition in package set: " <> toString rn

data MissingDeclaredPackageErr = MissingDeclaredPackageErr ProjectName PackageName
  deriving (Eq, Show)

instance Exception MissingDeclaredPackageErr where
  displayException (MissingDeclaredPackageErr jn pn) =
    "Missing package declared in project: " <> toString jn <> " " <> toString pn

newtype NoUniqueIpkgErr = NoUniqueIpkgErr Directory
  deriving (Eq, Show)

instance Exception NoUniqueIpkgErr where
  displayException (NoUniqueIpkgErr path) =
    "Could not find unique ipkg file in directory: " <> path

data UnsupportedCodegenExeErr = UnsupportedCodegenExeErr PackageName Codegen
  deriving (Eq, Show)

instance Exception UnsupportedCodegenExeErr where
  displayException (UnsupportedCodegenExeErr pn cg) =
    "Unsupported codegen " <> toString cg <> " - cannot run " <> toString pn

newtype MissingPackageInResolvedErr = MissingPackageInResolvedErr PackageName
  deriving (Eq, Show)

instance Exception MissingPackageInResolvedErr where
  displayException (MissingPackageInResolvedErr pn) =
    "Missing packaged in resolved set: " <> toString pn

data IpkgOverrideErr = IpkgOverrideErr PackageName FilePath PackageOverride
  deriving (Eq, Show)

instance Exception IpkgOverrideErr where
  displayException (IpkgOverrideErr pn ipkg override) =
    "Package set defines ipkg and override for " <> toString pn <> " " <> ipkg <> " " <> show override

findExtRel :: String -> Directory -> AppM [FilePath]
findExtRel ext dir = fmap (fmap (makeRelative dir)) (fsFindFiles (isExtensionOf ext) (Just dir))

-- | Reads out a package file (idr-package.json)
readPkgFile :: FilePath -> AppM Package
readPkgFile path = serdeReadJSON (PkgParseErr path) path

-- | Reads out a package set file (idr-package-set.json)
readPkgSetFile :: FilePath -> AppM PackageSet
readPkgSetFile path = serdeReadJSON (PkgSetParseErr path) path

-- | Reads out a project file (idr-project.json).
readProjFile :: FilePath -> AppM Project
readProjFile path = serdeReadJSON (ProjParseErr path) path

mkUniqueMap :: (Exception e, Ord k) => (k -> e) -> [(k, v)] -> AppM (Map k v)
mkUniqueMap f = foldr go (pure Map.empty) where
  go (k, v) mm = do
    m <- mm
    case Map.lookup k m of
      Nothing -> pure (Map.insert k v m)
      _ -> throwIO (f k)

-- | The Idris2 default codegen.
chezCodegen :: Codegen
chezCodegen = fromText "chez"

-- | The reference counting codegen
refcCodegen :: Codegen
refcCodegen = fromText "refc"

mkProgramSpec :: Directory -> Codegen -> PackageName -> AppM Spec
mkProgramSpec projDir codegen pn = do
  let part = toString pn
      path = projDir </> outputDir </> part
  case codegen of
    c | c == chezCodegen ->
      pure (Spec "sh" [part] (Just path) [])
    c | c == refcCodegen ->
      pure (Spec part [] (Just path) [])
    _ -> throwIO (UnsupportedCodegenExeErr pn codegen)

-- | Reads project and package info into one struct.
resolveProj :: Directory -> Project -> AppM ResolvedProject
resolveProj projDir (Project jn mcg mpaths) = do
  let cg = fromMaybe chezCodegen mcg
      paths = fromMaybe [] mpaths
  lps <- for paths $ \path -> do
    pkg <- readPkgFile (projDir </> path </> pkgFileName)
    let pn = packageName pkg
    pure (pn, LocatedPackage path pkg)
  pmap <- mkUniqueMap DuplicatePackageInSetErr lps
  pure (ResolvedProject jn cg pmap)

initRepoDeps :: PackageSet -> Deps PackageName RepoName
initRepoDeps (PackageSet _ pkgs projs) = depsFromEdges edges where
  edges = pkgEdges ++ projEdges
  pkgEdges = fmap mkPkgEdge (maybe [] Map.toList pkgs)
  projEdges = fromMaybe [] projs >>= mkProjEdge
  mkPkgEdge (name, PackageRef repo _ _ _ _) = (name, repo)
  mkProjEdge (ProjectRef repo _ pns) = fmap (, repo) pns

initPkgDeps :: ResolvedProject -> Deps PackageName PackageName
initPkgDeps (ResolvedProject _ _ pkgs) = depsFromGroups groups where
  groups = fmap (mkGroup . lpPkg . snd) (Map.toList pkgs)
  mkGroup (Package name _ _ depends) = (name, maybe Set.empty Set.fromList depends)

repoDeps :: ResolvedProject -> PackageSet -> Deps PackageName RepoName
repoDeps rp ps = composeDeps (closureDeps (initPkgDeps rp)) (initRepoDeps ps)

allRepos :: ResolvedProject -> PackageSet -> Set RepoName
allRepos rp ps = unionAllDeps (repoDeps rp ps)

specificRepos :: Foldable f => ResolvedProject -> PackageSet -> f PackageName -> Set RepoName
specificRepos rp ps pns = unionDeps pns (repoDeps rp ps)

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
    PackageGroupSubset pns -> "selected packages (" <> T.intercalate ", " (fmap toText (Set.toList pns)) <> ")"

reposForGroup :: ResolvedProject -> PackageSet -> PackageGroup -> Map RepoName RepoRef
reposForGroup rp ps g =
  let repos = case g of
        PackageGroupAll -> allRepos rp ps
        PackageGroupSubset pns -> specificRepos rp ps pns
      refs = fromMaybe Map.empty (psRepos ps)
  in Map.fromList (fmap (\r -> (r, refs Map.! r)) (Set.toList repos))

-- pkgDepsForGroup :: ResolvedProject -> PackageGroup -> Deps PackageName PackageName
-- pkgDepsForGroup rp g =
--   let ipd = initPkgDeps rp
--   in case g of
--     PackageGroupAll -> ipd
--     PackageGroupSubset s -> restrictDeps (`Set.member` s) ipd

depInfoPkgDeps :: DepInfoMap -> Deps PackageName PackageName
depInfoPkgDeps = depsFromGroups . fmap (fmap (Set.fromList . depInfoDepends)) . Map.toList

fullPkgDepsForGroup :: ResolvedProject -> PackageGroup -> DepInfoMap -> Deps PackageName PackageName
fullPkgDepsForGroup rp g dim =
  let y = depInfoPkgDeps dim
      z = case g of
            PackageGroupAll -> Map.keysSet (rpPackages rp)
            PackageGroupSubset s -> s
  in restrictDeps (`Set.member` z) y

readResolvedProject :: Directory -> AppM ResolvedProject
readResolvedProject projDir = do
  proj <- readProjFile (projDir </> projFileName)
  rp <- resolveProj projDir proj
  when (null (rpPackages rp)) $ do
    logWarning ("Project contains no packages yet."
               <> "Use `idream add` to add a package to this project.")
  pure rp

readDepInfoMap :: Directory -> ResolvedProject -> AppM DepInfoMap
readDepInfoMap projDir rp = do
  ps <- readPkgSetFile (projDir </> pkgSetFileName)
  mkDepInfoMap projDir rp ps

mkProjectDepInfo :: Bool -> Directory -> Package -> DepInfo
mkProjectDepInfo local path (Package _ mty msourcedir mdepends) = DepInfoIdream pdi where
  ty = fromMaybe PkgTypeLibrary mty
  depends = fromMaybe [] mdepends
  pdi = IdreamDepInfo local path ty msourcedir depends

mkLocalDepPair :: LocatedPackage -> (PackageName, DepInfo)
mkLocalDepPair (LocatedPackage path pkg) = (packageName pkg, mkProjectDepInfo True path pkg)

mkRemoteDepPair :: Directory -> LocatedPackage -> (PackageName, DepInfo)
mkRemoteDepPair subProjDir (LocatedPackage path pkg) = (packageName pkg, mkProjectDepInfo False (subProjDir </> path) pkg)

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
    _ -> repoFetchDir rn

mkPkgDepInfo :: Directory -> Map RepoName RepoRef -> PackageName -> PackageRef -> AppM DepInfo
mkPkgDepInfo projDir repoRefs pn (PackageRef rn msubdir mipkg moverride mdepends) = do
  case Map.lookup rn repoRefs of
    Nothing -> throwIO (MissingRepoInPackageSetErr rn)
    Just rr -> do
      let repoDir = getRepoDir rn rr
          path = maybe repoDir (repoDir </>) msubdir
          depends = fromMaybe [] mdepends
      case (mipkg, moverride) of
        (Just ipkg, Just override) -> throwIO (IpkgOverrideErr pn ipkg override)
        (Just ipkg, Nothing) ->
          pure (DepInfoIpkg (IpkgDepInfo path ipkg depends))
        (Nothing, Just (PackageOverride msourcedir)) ->
          pure (DepInfoIdream (IdreamDepInfo False path PkgTypeLibrary msourcedir depends))
        (Nothing, Nothing) -> do
          pkgCand <- findExtRel "ipkg" (projDir </> path)
          case filter (not . any isPathSeparator) pkgCand of
            [ipkg] -> pure (DepInfoIpkg (IpkgDepInfo path ipkg depends))
            _ -> throwIO (NoUniqueIpkgErr path)

mkPkgDepPair :: Directory -> Map RepoName RepoRef -> PackageName -> PackageRef -> AppM (PackageName, DepInfo)
mkPkgDepPair projDir repoRefs pn pr = fmap (pn,) (mkPkgDepInfo projDir repoRefs pn pr)

mkProjDepPairs :: Directory -> Map RepoName RepoRef -> ProjectRef -> AppM [(PackageName, DepInfo)]
mkProjDepPairs projDir repoRefs (ProjectRef rn msubdir pkgs) = do
  case Map.lookup rn repoRefs of
    Nothing -> throwIO (MissingRepoInPackageSetErr rn)
    Just rr -> do
      let repoDir = getRepoDir rn rr
          path = maybe repoDir (repoDir </>) msubdir
          subProjDir = projDir </> path
          subProjFile = subProjDir </> projFileName
      subProj <- readProjFile subProjFile
      subResolvedProj <- resolveProj subProjDir subProj
      let pmap = rpPackages subResolvedProj
      for_ pkgs $ \pn -> unless (Map.member pn pmap) (throwIO (MissingDeclaredPackageErr (projectName subProj) pn))
      pure (fmap (mkRemoteDepPair path) (Map.elems pmap))

mkDepInfoMap :: Directory -> ResolvedProject -> PackageSet -> AppM DepInfoMap
mkDepInfoMap projDir rp ps = do
  let localPairs = fmap mkLocalDepPair (Map.elems (rpPackages rp))
      builtinPairs = mkBuiltinDepPairs
      repoRefs = fromMaybe Map.empty (psRepos ps)
      pkgRefs = maybe [] Map.toList (psPkgs ps)
      projRefs = fromMaybe [] (psProjects ps)
  pkgPairs <- for pkgRefs (uncurry (mkPkgDepPair projDir repoRefs))
  projPairs <- fmap join (for projRefs (mkProjDepPairs projDir repoRefs))
  let allPairs = localPairs ++ builtinPairs ++ pkgPairs ++ projPairs
  mkUniqueMap DuplicatePackageInResolvedErr allPairs

getDepInfoMap :: PackageName -> DepInfoMap -> AppM DepInfo
getDepInfoMap pn dim =
  case Map.lookup pn dim of
    Nothing -> throwIO (MissingPackageInResolvedErr pn)
    Just di -> pure di
