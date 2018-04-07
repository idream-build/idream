
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Idream.Command.GenerateIpkg ( generateIpkgFile ) where


-- Imports

import Idream.Log ( MonadLogger )
import Idream.SafeIO
import qualified Idream.Log as Log
import Control.Exception ( IOException )
import Idream.Types
import Idream.Graph
import qualified Algebra.Graph as Graph
import Idream.Command.Common ( readRootProjFile, readPkgFile
                             , getPkgFilePath, getPkgDirPath, handleReadPkgErr
                             , ReadProjectErr(..), ReadPkgErr(..) )
import Idream.FileSystem
import System.Directory ( removePathForcibly, createDirectoryIfMissing )
import Shelly ( shelly, silently, hasExt, chdir, cp_r, findWhen
              , fromText, toTextIgnore )
import Data.Text ( Text )
import Data.Monoid ( (<>) )
import Data.List ( intercalate )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


-- Data types

-- | Type alias for modules inside an ipkg file.
type Module = FilePath

-- | Data type containing all info needed to generate an ipkg file.
data IpkgMetadata = IpkgMetadata Package [Module]
                  deriving (Eq, Show)

-- | Error type used for storing all errors when generating .ipkg files.
data GenerateIpkgErr = GProjFileReadErr ReadProjectErr
                     | GLoadGraphErr GraphErr
                     | GCopyFilesErr PackageName IOException
                     | GFindPkgFilesErr PackageName IOException
                     | GReadPkgFileErr ReadPkgErr
                     | GGenerateIpkgErr PackageName IOException
                     deriving (Eq, Show)


-- Functions

-- | Top level function used for generating .ipkg files.
generateIpkgFile :: ( MonadLogger m, MonadIO m ) => m ()
generateIpkgFile = do
  result <- runSafeIO $ do
    Project _ rootPkgs <- readRootProjFile GProjFileReadErr
    if null rootPkgs
      then Log.info ("Project contains no packages yet, skipping generate step. "
                  <> "Use `idream add` to add a package to this project first.")
      else do
        Log.info "Generating .ipkg files..."
        graph <- loadGraphFromJSON GLoadGraphErr depGraphFile
        mapM_ generateIpkg $ Graph.vertexList graph
  case result of
    Left err -> handleGenerateIpkgErr err
    Right _ -> Log.info "Finished generating .ipkg files."

-- | Generates an ipkg file for 1 package in a project.
--   Note that this also cleans the build directory for that project.
generateIpkg :: ( MonadLogger m, MonadSafeIO GenerateIpkgErr m )
             => DepNode -> m ()
generateIpkg node@(DepNode pkgName@(PackageName name) prjName@(ProjectName projName)) = do
  Log.debug ("Generating ipkg file for package: " <> name <> ".")
  pkgDirPath <- getPkgDirPath GProjFileReadErr pkgName prjName
  let pkgName' = T.unpack name
      pkgBuildDir = buildDir </> "build" </> T.unpack projName
      toFilePath = fromText . T.pack
  liftSafeIO (GCopyFilesErr pkgName) $ do
    removePathForcibly $ pkgBuildDir </> pkgName'
    createDirectoryIfMissing True pkgBuildDir
    shelly $ silently $ cp_r (toFilePath pkgDirPath) (toFilePath pkgBuildDir)
  generateIpkgHelper node (pkgBuildDir </> T.unpack name)


-- | Helper function that does the actual generation of the .ipkg file.
generateIpkgHelper :: ( MonadLogger m, MonadSafeIO GenerateIpkgErr m )
                   => DepNode -> Directory -> m ()
generateIpkgHelper (DepNode pkgName@(PackageName name) projName) pkgBuildDir = do
  pkgFilePath <- getPkgFilePath GProjFileReadErr pkgName projName
  package@(Package _ _ (SourceDir srcDir) _) <- readPkgFile GReadPkgFileErr pkgFilePath
  idrisFiles <- findIdrisFiles pkgName $ pkgBuildDir </> srcDir
  let pkgMetadata = IpkgMetadata package idrisFiles
      contents = ipkgMetadataToText pkgMetadata
      ipkgFile = pkgBuildDir </> T.unpack name <> ".ipkg"
  Log.debug ("Writing .ipkg file to: " <> T.pack ipkgFile)
  liftSafeIO (GGenerateIpkgErr pkgName) $ TIO.writeFile ipkgFile contents


-- | Helper function that returns all idris files (ending in .idr) in a directory.
findIdrisFiles :: MonadSafeIO GenerateIpkgErr m
               => PackageName -> Directory -> m [Module]
findIdrisFiles pkgName dir =
  let toFilePath = fromText . T.pack
      fromFilePath = T.unpack . toTextIgnore
      dir' = toFilePath dir
      isIdrisFile = pure . hasExt "idr"
  in liftSafeIO (GFindPkgFilesErr pkgName) $ do
    idrisFiles <- shelly $ silently $ chdir dir' $ findWhen isIdrisFile "."
    return $ fromFilePath <$> idrisFiles

-- | Converts the ipkg metadata to a text representation.
ipkgMetadataToText :: IpkgMetadata -> Text
ipkgMetadataToText (IpkgMetadata pkg modules) =
  let (Package pkgName pkgType (SourceDir srcDir) dependencies) = pkg
      name = unPkgName pkgName
      mods = T.pack $ intercalate ", " $ formatFileNames modules
      deps = T.intercalate ", " $ unProjName <$> dependencies
      sourceDir = T.pack srcDir
  in T.unlines [ "package " <> name
               , "-- NOTE: This is an auto-generated file by idream. Do not edit."
               , "modules = " <> mods
               , if null dependencies then "" else "pkgs = " <> deps
               , "sourcedir = " <> sourceDir
               , if pkgType == Executable then "executable = " <> name else ""
               , if pkgType == Executable then "main = Main" else ""
               ]

-- | Formats the filename for use in ipkg file.
--   e.g. ./LightYear/Position.idr -> LightYear.Position
formatFileNames :: [String] -> [String]
formatFileNames modules = replaceSlashes . trimLeadingSlash . trimExt <$> modules
  where replaceSlash '/' = '.'
        replaceSlash c = c
        trimLeadingSlash = drop 2
        trimExt s = take (length s - 4) s
        replaceSlashes = fmap replaceSlash

-- | Helper function for handling errors when generating ipkg files.
handleGenerateIpkgErr :: MonadLogger m => GenerateIpkgErr -> m ()
handleGenerateIpkgErr (GProjFileReadErr err) =
  Log.err ("Failed to read root project file: "
           <> T.pack (show err))
handleGenerateIpkgErr (GLoadGraphErr err) = handleGraphErr err
handleGenerateIpkgErr (GCopyFilesErr pkgName err) =
  Log.err ("Failed to copy files into build directory for package "
           <> unPkgName pkgName <> ", reason: "
           <> T.pack (show err) <> ".")
handleGenerateIpkgErr (GFindPkgFilesErr pkgName err) =
  Log.err ("Failed to find Idris files for package "
           <> unPkgName pkgName <> ", reason: "
           <> T.pack (show err) <> ".")
handleGenerateIpkgErr (GReadPkgFileErr err) = handleReadPkgErr err
handleGenerateIpkgErr (GGenerateIpkgErr pkgName err) =
  Log.err ("Failed to generate .ipkg file for package"
           <> unPkgName pkgName <> ", reason: "
           <> T.pack (show err) <> ".")

