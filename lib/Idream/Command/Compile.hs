
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Compile ( compileCode ) where

-- Imports

import Control.Monad.Reader
import Idream.SafeIO
import Idream.Log ( MonadLogger )
import qualified Idream.Log as Log
import Idream.Command.Common ( invokeCmdWithEnv )
import Idream.Types ( PackageName(..), ProjectName(..), Config
                    , Directory, buildDir, buildSettings )
import Idream.Graph ( DepNode(..), BuildPlan, GraphErr
                    , loadGraphFromJSON, createBuildPlan
                    , handleGraphErr )
import System.FilePath ( (</>) )
import System.Directory ( createDirectoryIfMissing )
import System.Exit ( ExitCode(..) )
import Shelly ( shelly, silently, cp_r, fromText )
import Control.Exception ( IOException )
import qualified Data.Text as T
import Data.Monoid


-- Data types

data CompileErr = CLoadGraphErr GraphErr
                | CCreateCompileDirErr IOException
                | CCopyFilesErr PackageName IOException
                | CIdrisInvokeErr PackageName Int
                | CIdrisCommandErr IOException
                deriving (Eq, Show)


-- Functions


compileCode :: ( MonadReader Config m, MonadLogger m, MonadIO m ) => m ()
compileCode = do
  Log.info "Compiling package(s)..."
  workDir <- asks $ buildDir . buildSettings
  let graphFile = workDir </> "dependency-graph.json"
  result <- runSafeIO $ do
    graph <- loadGraphFromJSON CLoadGraphErr graphFile
    let buildPlan = createBuildPlan graph
    compilePackages buildPlan
  case result of
    Left err -> handleCompileErr err
    Right _ -> Log.info "Successfully compiled package(s)!"


compilePackages :: ( MonadReader Config m, MonadLogger m, MonadSafeIO CompileErr m )
                => BuildPlan DepNode -> m ()
compilePackages = mapM_ compilePackage where  -- TODO optimize / parallellize...
  compilePackage depNode@(DepNode pkgName _) = do
    let name = unPkgName pkgName
    Log.debug ("Compiling package: " <> name)
    result <- idrisCompile depNode
    case result of
      ExitSuccess -> Log.info ("Successfully compiled " <> name <> ".")
      ExitFailure code -> raiseError $ CIdrisInvokeErr pkgName code


idrisCompile :: ( MonadReader Config m, MonadLogger m, MonadSafeIO CompileErr m )
             => DepNode -> m ExitCode
idrisCompile node@(DepNode pn@(PackageName pkgName) _) = do
  workDir <- asks $ buildDir . buildSettings
  let compileDir = compilationDir workDir node
      idrisArgs = [ "--verbose"
                  , "--build"
                  , T.unpack $ pkgName <> ".ipkg"  -- TODO correct dir?
                  ]
      environ = [ ("IDRIS_LIBRARY_PATH", compileDir)
                , ("IDRIS_DOC_PATH", documentationDir workDir node)
                ]
  let downloadDir = fromText . T.pack $ downloadPackageDir workDir node
      workDir' = fromText . T.pack $ packageDir workDir node
  liftSafeIO CCreateCompileDirErr $ createDirectoryIfMissing True compileDir
  liftSafeIO (CCopyFilesErr pn) $ shelly $ silently $ cp_r downloadDir workDir'
  invokeCmdWithEnv CIdrisCommandErr "idris" idrisArgs environ


downloadPackageDir :: Directory -> DepNode -> Directory
downloadPackageDir dir (DepNode (PackageName pkgName) projName) =
  downloadProjectDir dir projName </> T.unpack pkgName


downloadProjectDir :: Directory -> ProjectName -> Directory
downloadProjectDir dir (ProjectName projName) =
  dir </> "src" </> T.unpack projName


packageDir :: Directory -> DepNode -> Directory
packageDir dir (DepNode (PackageName pkgName) (ProjectName projName)) =
  dir </> "build" </> T.unpack projName </> T.unpack pkgName


compilationDir :: Directory -> DepNode -> Directory
compilationDir dir node = packageDir dir node </> "compiled"


documentationDir :: Directory -> DepNode -> Directory
documentationDir dir node = packageDir dir node </> "docs"


handleCompileErr :: MonadLogger m => CompileErr -> m ()
handleCompileErr (CLoadGraphErr err) = handleGraphErr err
handleCompileErr (CCopyFilesErr (PackageName name) err) =
  Log.err ("Failed to copy files for package " <> name
              <> "from src directory into build directory: "
              <> T.pack (show err) <> ".")
handleCompileErr (CCreateCompileDirErr err) =
  Log.err ("Failed to create directory: " <> T.pack (show err))
handleCompileErr (CIdrisInvokeErr pkgName err) =
  Log.err ("Failed to invoke `idris` when compiling packages: exit-code = "
        <> T.pack (show err) <> " when building package "
        <> unPkgName pkgName <> ".")
handleCompileErr (CIdrisCommandErr err) =
  Log.err ("Exception occurred when invoking `idris`: "
        <> T.pack (show err) <> ".")

