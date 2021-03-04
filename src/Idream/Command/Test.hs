module Idream.Command.Test
  ( testImpl
  , NonTestPackagesErr (..)
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Idream.Command.Common (mkProgramSpec, pkgGroupToText, readDepInfoMap, readResolvedProject)
import Idream.Effects.FileSystem (fsFindFiles)
import Idream.Effects.Process (procInvokeEnsure_)
import Idream.FileLogic (outputDir)
import Idream.Prelude
import Idream.Types.Common (PackageGroup (..), PackageName, PackageType (..), ProjectName)
import Idream.Types.External (Package (..))
import Idream.Types.Internal (DepInfo (..), DepInfoMap, IdreamDepInfo (..), LocatedPackage (..), ResolvedProject (..))

newtype NonTestPackagesErr = NonTestPackagesErr (Set PackageName)
  deriving (Eq, Show)

instance Exception NonTestPackagesErr where
  displayException (NonTestPackagesErr pkgs) =
    "Non-test packages specified in test group: " <> intercalate ", " (fmap toString (Set.toList pkgs))

testImpl :: Directory -> PackageGroup -> AppM ()
testImpl projDir group = do
  rp <- readResolvedProject projDir
  logInfo ("Testing project " <> toText (rpName rp) <> " with " <> pkgGroupToText group)
  dim <- readDepInfoMap projDir rp
  let localTestMap = mkLocalTestMap rp
      allTestMap = mkAllTestMap dim
  testMap <- case group of
    PackageGroupAll -> pure localTestMap
    PackageGroupSubset reqNames -> do
      let allNames = Map.keysSet allTestMap
          invalidNames = Set.difference reqNames allNames
      unless (Set.null invalidNames) (throwIO (NonTestPackagesErr invalidNames))
      pure (Map.filterWithKey (\pn _ -> Set.member pn reqNames) allTestMap)
  for_ (Map.toList testMap) $ \(pn, _) -> do
    logInfo ("Running test " <> toText pn)
    let codegen = rpCodegen rp
    spec <- mkProgramSpec projDir codegen pn
    procInvokeEnsure_ spec
  logInfo "Finished testing"

mkLocalTestMap :: ResolvedProject -> Map PackageName Directory
mkLocalTestMap rp = Map.fromList $ do
  LocatedPackage path (Package pn mty _ _) <- Map.elems (rpPackages rp)
  case mty of
    Just PkgTypeTest -> [(pn, path)]
    _ -> []

mkAllTestMap :: DepInfoMap -> Map PackageName Directory
mkAllTestMap = Map.fromList . (>>= go) . Map.toList where
  go (pn, di) =
    case di of
      DepInfoIdream (IdreamDepInfo _ path ty _ _) | ty == PkgTypeTest -> [(pn, path)]
      _ -> []
