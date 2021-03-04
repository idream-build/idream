module Idream.Command.Test
  ( testImpl
  , NonTestPackagesErr (..)
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Idream.Command.Common (pkgGroupToText, readDepInfoMap, readResolvedProject)
import Idream.Command.Compile (compileInner)
import Idream.Command.Fetch (fetchInner)
import Idream.Prelude
import Idream.Types.Common (PackageGroup (..), PackageName (..), PackageType (..), ProjectName (..), RefreshStrategy)
import Idream.Types.External (Package (..))
import Idream.Types.Internal (DepInfo (..), DepInfoMap (..), IdreamDepInfo (..), LocatedPackage (..),
                              ResolvedProject (..))

newtype NonTestPackagesErr = NonTestPackagesErr (Set PackageName)
  deriving (Eq, Show)

instance Exception NonTestPackagesErr where
  displayException (NonTestPackagesErr pkgs) =
    "Non-test packages specified in test group: " <> intercalate ", " (fmap (T.unpack . unPkgName) (Set.toList pkgs))

testImpl :: Directory -> PackageGroup -> RefreshStrategy -> AppM ()
testImpl projDir group refreshStrat = do
  rp <- readResolvedProject projDir
  logInfo ("Testing project " <> unProjName (rpName rp) <> " with " <> pkgGroupToText group <> ".")
  fetchInner projDir rp group refreshStrat
  dim <- readDepInfoMap projDir rp
  let localTestPkgNames = listLocalTestPkgNames rp
      allTestPkgNames = listAllTestPkgNames dim
  -- realGroup <- case group of
  --   PackageGroupAll -> pure localTestPkgNames
  --   PackageGroupSubset reqNames -> do
  --     let invalidNames = Set.difference reqNames allTestPkgNames
  --     unless (Set.null invalidNames) (throwIO (NonTestPackagesErr invalidNames))
  --     pure reqNames
  -- compileInner projDir rp realGroup dim
  error "TODO - finish test"

listLocalTestPkgNames :: ResolvedProject -> Set PackageName
listLocalTestPkgNames rp =
  let allPkgs = fmap lpPkg (Map.elems (rpPackages rp))
      testPkgs = filter (\p -> packageType p == Just PkgTypeTest) allPkgs
      pkgNames = fmap packageName testPkgs
  in Set.fromList pkgNames

listAllTestPkgNames :: DepInfoMap -> Set PackageName
listAllTestPkgNames = Set.fromList . (>>= go) . Map.toList . unDepInfoMap where
  go (pn, di) =
    case di of
      DepInfoIdream idi | idreamDepType idi == PkgTypeTest -> [pn]
      _ -> []
