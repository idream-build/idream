module Idream.Command.Repl
  ( replImpl
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Idream.Prelude
import Idream.Effects.FileSystem (fsMakeAbsolute)
import Idream.Effects.Process (Spec (..), procDebug_)
import Idream.Types.Common (Codegen, PackageName, PackageGroup (PackageGroupSubset))
import Idream.FileLogic (workDir, buildDirName, outputDirName, installDirName)
import Idream.Types.Internal (DepInfo (..), IpkgDepInfo (..), IdreamDepInfo (..), ResolvedProject (rpCodegen))
import Idream.Command.Common (readResolvedProject, readDepInfoMap, fullPkgDepsForGroup)
import Idream.Deps (closureDeps, lookupDeps)
import UnliftIO.Environment (getEnv)

replImpl :: Directory -> PackageName -> AppM ()
replImpl projDir pn = do
  logInfo ("Entering repl for package " <> toText pn)
  rp <- readResolvedProject projDir
  dim <- readDepInfoMap projDir rp
  let filtDeps = fullPkgDepsForGroup rp (PackageGroupSubset (Set.singleton pn)) dim
      transDeps = closureDeps filtDeps
      tdepends = Set.toList (lookupDeps pn transDeps)
      codegen = rpCodegen rp
  case Map.lookup pn dim of
    Nothing -> error "TODO - throw appropriate error"
    Just di -> replPkg projDir codegen di pn tdepends

replPkg :: Directory -> Codegen -> DepInfo -> PackageName -> [PackageName] -> AppM ()
replPkg projDir codegen di pn tdepends = do
  case di of
    DepInfoBuiltin _ -> error "TODO - throw appropriate error"
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
      env = [ ("IDRIS2_PATH", idpath)
            , ("PATH", shellPath)
            , ("TERM", "xterm-256color")
            ]
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
