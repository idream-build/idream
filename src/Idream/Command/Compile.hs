module Idream.Command.Compile
  ( compile
  ) where

import Idream.App (AppM)
import Idream.FilePaths (Directory)
import Idream.Command.Common (PackageGroup)

compile :: Directory -> PackageGroup -> AppM ()
compile = undefined

-- import Data.Foldable (for_)
-- import qualified Data.Text as T
-- import Idream.App (AppM)
-- import Idream.Command.Common (readRootProjFile)
-- import Idream.Effects.FileSystem (fsCopyDir, fsCreateDir)
-- import Idream.Effects.Idris (idrisCompile, idrisGetLibDir)
-- import Idream.FileLogic (compileDir, depGraphFile)
-- import Idream.FilePaths (Directory)
-- import Idream.Graph (BuildPlan, DepNode (..), createBuildPlan, loadGraphFromJSON)
-- import Idream.ToText (ToText (..))
-- import Idream.Types (Project (..))
-- import LittleLogger (logDebug, logInfo)
-- import System.FilePath ((</>))

-- -- | Top level function for compiling Idris packages.
-- compileCode :: AppM ()
-- compileCode = do
--   Project _ rootPkgs <- readRootProjFile
--   if null rootPkgs
--     then logInfo ("Project contains no packages yet, skipping compile step. "
--                 <> "Use `idream add` to add a package to this project first.")
--     else do
--       logInfo "Compiling package(s)..."
--       graph <- loadGraphFromJSON depGraphFile
--       let buildPlan = createBuildPlan graph
--       compilePackages buildPlan
--       logInfo "Successfully compiled package(s)!"

-- -- | Performs the actual compilation of the Idris packages.
-- --   Takes the buildplan into account to properly build dependencies in order.
-- compilePackages :: BuildPlan DepNode -> AppM ()
-- compilePackages buildPlan = do
--   libDir <- idrisGetLibDir
--   fsCreateDir compileDir
--   logDebug "Copying files from base packages into compile directory."
--   setupBasePackages libDir
--   for_ buildPlan compilePackage  -- TODO optimize / parallellize, handle deps...
--   where compilePackage (DepNode pkgName projName) = do
--           logDebug ("Compiling package: " <> toText pkgName <> ".")
--           idrisCompile projName pkgName
--           logInfo ("Compiled package: " <> toText pkgName <> ".")

-- -- Copies over the 'standard' packages used by Idris into the main compilation directory.
-- setupBasePackages :: Directory -> AppM ()
-- setupBasePackages libDir = mapM_ setupBasePackage basePackages where
--   basePackages = ["base", "contrib", "network", "prelude"]
--   setupBasePackage pkg = do
--     let fromDir = libDir </> T.unpack pkg
--     fsCopyDir fromDir compileDir
