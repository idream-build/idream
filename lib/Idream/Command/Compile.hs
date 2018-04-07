
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Compile ( compileCode ) where

-- Imports

import Control.Monad.Reader
import Idream.SafeIO
import Idream.Log ( MonadLogger )
import qualified Idream.Log as Log
import Idream.Command.Common ( invokeCmdWithEnv, readRootProjFile
                             , ReadProjectErr(..) )
import Idream.Types ( PackageName(..), ProjectName(..), Project(..) )
import Idream.Graph ( DepNode(..), BuildPlan, GraphErr
                    , loadGraphFromJSON, createBuildPlan
                    , handleGraphErr )
import Idream.FileSystem
import System.FilePath ( (</>) )
import System.Directory ( createDirectoryIfMissing )
import System.Exit ( ExitCode(..) )
import Control.Exception ( IOException )
import qualified Data.Text as T
import Data.Monoid


-- Data types

data CompileErr = CProjReadFileErr ReadProjectErr
                | CLoadGraphErr GraphErr
                | CCreateCompileDirErr IOException
                | CIdrisInvokeErr PackageName Int
                | CIdrisCommandErr IOException
                deriving (Eq, Show)


-- Functions


compileCode :: ( MonadLogger m, MonadIO m ) => m ()
compileCode = do
  result <- runSafeIO $ do
    Project _ rootPkgs <- readRootProjFile CProjReadFileErr
    if null rootPkgs
      then Log.info ("Project contains no packages yet, skipping compile step. "
                  <> "Use `idream add` to add a package to this project first.")
      else do
        Log.info "Compiling package(s)..."
        graph <- loadGraphFromJSON CLoadGraphErr depGraphFile
        let buildPlan = createBuildPlan graph
        compilePackages buildPlan
  case result of
    Left err -> handleCompileErr err
    Right _ -> Log.info "Successfully compiled package(s)!"


compilePackages :: ( MonadLogger m, MonadSafeIO CompileErr m )
                => BuildPlan DepNode -> m ()
compilePackages = mapM_ compilePackage where
  -- TODO optimize / parallellize, handle deps...
  compilePackage depNode@(DepNode pkgName _) = do
    let name = unPkgName pkgName
    Log.debug ("Compiling package: " <> name)
    result <- idrisCompile depNode
    case result of
      ExitSuccess -> Log.info ("Successfully compiled " <> name <> ".")
      ExitFailure code -> raiseError $ CIdrisInvokeErr pkgName code


idrisCompile :: ( MonadLogger m, MonadSafeIO CompileErr m )
             => DepNode -> m ExitCode
idrisCompile node@(DepNode (PackageName pkgName) _) = do
  let compileDir = compilationDir buildDir node
      ipkgFile = packageDir buildDir node </> T.unpack (pkgName <> ".ipkg")
      idrisArgs = [ "--verbose", "--build", ipkgFile]
      environ = [ ("IDRIS_LIBRARY_PATH", compileDir)
                , ("IDRIS_DOC_PATH", documentationDir buildDir node)
                ]
  liftSafeIO CCreateCompileDirErr $ createDirectoryIfMissing True compileDir
  invokeCmdWithEnv CIdrisCommandErr "idris" idrisArgs environ


packageDir :: Directory -> DepNode -> Directory
packageDir dir (DepNode (PackageName pkgName) (ProjectName projName)) =
  dir </> "build" </> T.unpack projName </> T.unpack pkgName


compilationDir :: Directory -> DepNode -> Directory
compilationDir dir node = packageDir dir node </> "bin"


documentationDir :: Directory -> DepNode -> Directory
documentationDir dir node = packageDir dir node </> "docs"


handleCompileErr :: MonadLogger m => CompileErr -> m ()
handleCompileErr (CProjReadFileErr err) =
  Log.err ("Failed to read root project file: "
          <> T.pack (show err))
handleCompileErr (CLoadGraphErr err) = handleGraphErr err
handleCompileErr (CCreateCompileDirErr err) =
  Log.err ("Failed to create directory: " <> T.pack (show err))
handleCompileErr (CIdrisInvokeErr pkgName err) =
  Log.err ("Failed to invoke `idris` when compiling packages: exit-code = "
        <> T.pack (show err) <> " when building package "
        <> unPkgName pkgName <> ".")
handleCompileErr (CIdrisCommandErr err) =
  Log.err ("Exception occurred when invoking `idris`: "
        <> T.pack (show err) <> ".")

