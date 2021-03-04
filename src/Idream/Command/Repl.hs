module Idream.Command.Repl
  ( replImpl
  , ReplBuiltinPackageErr (..)
  ) where

import qualified Data.Set as Set
import Idream.Command.Common (fullPkgDepsForGroup, getDepInfoMap, readDepInfoMap, readResolvedProject)
import Idream.Deps (closureDeps, lookupDeps)
import Idream.Effects.FileSystem (fsMakeAbsolute)
import Idream.Effects.Process (Spec (..), procDebug_)
import Idream.FileLogic (buildDirName, installDirName, outputDirName, workDir)
import Idream.Prelude
import Idream.Types.Common (Codegen, PackageGroup (..), PackageName)
import Idream.Types.Internal (DepInfo (..), IdreamDepInfo (..), IpkgDepInfo (..), ResolvedProject (..))
import UnliftIO.Environment (getEnv)

newtype ReplBuiltinPackageErr = ReplBuiltinPackageErr PackageName
  deriving (Eq, Show)

instance Exception ReplBuiltinPackageErr where
  displayException (ReplBuiltinPackageErr pn) =
    "Cannot open repl for builtin package: " <> toString pn

replImpl :: Directory -> PackageName -> AppM ()
replImpl projDir pn = do
  logInfo ("Entering repl for package " <> toText pn)
  rp <- readResolvedProject projDir
  dim <- readDepInfoMap projDir rp
  let filtDeps = fullPkgDepsForGroup rp (PackageGroupSubset (Set.singleton pn)) dim
      transDeps = closureDeps filtDeps
      tdepends = Set.toList (lookupDeps pn transDeps)
      codegen = rpCodegen rp
  di <- getDepInfoMap pn dim
  replPkg projDir codegen di pn tdepends

replPkg :: Directory -> Codegen -> DepInfo -> PackageName -> [PackageName] -> AppM ()
replPkg projDir codegen di pn tdepends = do
  case di of
    DepInfoBuiltin _ -> throwIO (ReplBuiltinPackageErr pn)
    DepInfoIdream idi -> do
      let path = idreamDepPath idi
          pkgFile = toString pn -<.> "ipkg"
      idrisRepl projDir codegen pn path pkgFile tdepends
    DepInfoIpkg (IpkgDepInfo path pkgFile _) -> do
      idrisRepl projDir codegen pn path pkgFile tdepends

idrisRepl :: Directory -> Codegen -> PackageName -> Directory -> FilePath -> [PackageName] -> AppM ()
idrisRepl projDir codegen pn path pkgFile tdepends = do
  absWorkDir <- fsMakeAbsolute (projDir </> workDir)
  shellPath <- getEnv "PATH"
  let absBuildDir = absWorkDir </> buildDirName
      absOutputDir = absWorkDir </> outputDirName
      absInstallDir = absWorkDir </> installDirName
      depNames = fmap toString tdepends
      idpath = intercalate ":" (fmap (absInstallDir </>) depNames)
      idpathEnv = [("IDRIS2_PATH", idpath) | not (null idpath)]
      env = [ ("PATH", shellPath)
            , ("TERM", "xterm-256color")
            ] ++ idpathEnv
      pkgDirPart = toString pn
      absPkgBuildDir = absBuildDir </> pkgDirPart
      absPkgOutputDir = absOutputDir </> pkgDirPart
      args = [ "idris2"
             , "--repl", pkgFile
             , "--build-dir", absPkgBuildDir
             , "--output-dir", absPkgOutputDir
             , "--codegen", toString codegen
             , "--verbose"
             ]
      spec = Spec "rlwrap" args (Just (projDir </> path)) env
  procDebug_ spec
