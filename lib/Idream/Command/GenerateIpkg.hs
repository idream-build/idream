
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.GenerateIpkg ( generateIpkgFile ) where


-- Imports

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Exception ( IOException )
import Idream.Types
import Idream.Graph
import Idream.Command.Common ( tryAction, readPkgFile
                             , handleReadPkgErr, ReadPkgErr(..) )
import System.FilePath ( (</>) )
import System.Directory ( removePathForcibly, createDirectory )
import Shelly ( shelly, silently, hasExt, cp_r, findWhen
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
data GenerateIpkgErr = GLoadGraphErr GraphErr
                     | GCopyFilesErr PackageName IOException
                     | GFindPkgFilesErr PackageName IOException
                     | GReadPkgFileErr ReadPkgErr
                     | GGenerateIpkgErr PackageName IOException
                     deriving (Eq, Show)


-- Functions

-- | Top level function used for generating .ipkg files.
generateIpkgFile :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
generateIpkgFile = do
  $(logInfo) "Generating .ipkg files..."
  workDir <- asks $ buildDir . buildSettings
  let graphFile = workDir </> "dependency-graph.json"
  result <- runExceptT $ do
    graph <- withExceptT GLoadGraphErr $ loadGraphFromJSON graphFile
    mapM_ generateIpkg graph
  case result of
    Left err -> handleGenerateIpkgErr err
    Right _ -> $(logInfo) "Successfully generated .ipkg files!"

-- | Generates an ipkg file for 1 package in a project.
generateIpkg :: ( MonadError GenerateIpkgErr m
                , MonadReader Config m
                , MonadLogger m
                , MonadIO m )
             => DepNode -> m ()
generateIpkg node@(DepNode pkgName@(PackageName name) (ProjectName projName)) = do
  $(logDebug) ("Generating ipkg file for package: " <> name <> ".")
  workDir <- asks $ buildDir . buildSettings
  let pkgSrcDir = workDir </> "src" </> T.unpack projName </> T.unpack name
      pkgBuildDir = workDir </> "build" </> T.unpack projName </> T.unpack name
      toFilePath = fromText . T.pack
  tryAction (GCopyFilesErr pkgName) $ do
    removePathForcibly pkgBuildDir
    createDirectory pkgBuildDir
    shelly $ silently $ cp_r (toFilePath pkgSrcDir) (toFilePath pkgBuildDir)
  generateIpkgHelper node pkgBuildDir

-- | Helper function that does the actual generation of the .ipkg file.
generateIpkgHelper :: ( MonadError GenerateIpkgErr m
                      , MonadReader Config m
                      , MonadLogger m
                      , MonadIO m )
                   => DepNode -> Directory -> m ()
generateIpkgHelper (DepNode pkgName@(PackageName name) _) pkgDir = do
  pkgFilePath <- asks $ pkgFile . buildSettings
  let pkgFileName = pkgDir </> pkgFilePath
      ipkgFile = pkgDir </> T.unpack name <> ".ipkg"
  packageResult <- runExceptT $ readPkgFile pkgFileName
  case packageResult of
    Left err -> throwError $ GReadPkgFileErr err
    Right package@(Package _ _ (SourceDir srcDir) _) -> do
      idrisFiles <- findIdrisFiles pkgName $ pkgDir </> srcDir
      let pkgMetadata = IpkgMetadata package idrisFiles
          contents = ipkgMetadataToText pkgMetadata
      tryAction (GGenerateIpkgErr pkgName) $ TIO.writeFile ipkgFile contents

-- | Helper function that returns all idris files (ending in .idr) in a directory.
findIdrisFiles :: ( MonadError GenerateIpkgErr m
                  , MonadIO m )
               => PackageName -> Directory -> m [Module]
findIdrisFiles pkgName dir =
  let toFilePath = fromText . T.pack
      fromFilePath = T.unpack . toTextIgnore
      dir' = toFilePath dir
      isIdrisFile = pure . hasExt ".idr"
  in tryAction (GFindPkgFilesErr pkgName) $ do
    idrisFiles <- shelly $ silently $ findWhen isIdrisFile dir'
    return $ fromFilePath <$> idrisFiles

-- | Converts the ipkg metadata to a text representation.
ipkgMetadataToText :: IpkgMetadata -> Text
ipkgMetadataToText (IpkgMetadata pkg modules) =
  let (Package pkgName pkgType (SourceDir srcDir) subProjects) = pkg
      name = unPkgName pkgName
      mods = T.pack $ intercalate "," modules
      deps = T.intercalate "," $ unProjName <$> subProjects
      sourceDir = T.pack srcDir
  in T.unlines [ "-- NOTE: This is an auto-generated file by idream. Do not edit."
               , "package " <> name
               , "modules = " <> mods
               , "pkgs = " <> deps
               , "sourcedir =" <> sourceDir
               , if pkgType == Executable then "executable = " <> name else ""
               , if pkgType == Executable then "main = Main" else ""
               ]

-- | Helper function for handling errors when generating ipkg files.
handleGenerateIpkgErr :: MonadLogger m => GenerateIpkgErr -> m ()
handleGenerateIpkgErr (GLoadGraphErr err) = handleGraphErr err
handleGenerateIpkgErr (GCopyFilesErr pkgName err) =
  $(logError) ("Failed to copy files into build directory for package "
               <> unPkgName pkgName <> ", reason: "
               <> T.pack (show err) <> ".")
handleGenerateIpkgErr (GFindPkgFilesErr pkgName err) =
  $(logError) ("Failed to find Idris files for package "
               <> unPkgName pkgName <> ", reason: "
               <> T.pack (show err) <> ".")
handleGenerateIpkgErr (GReadPkgFileErr err) = handleReadPkgErr err
handleGenerateIpkgErr (GGenerateIpkgErr pkgName err) =
  $(logError) ("Failed to generate .ipkg file for package"
              <> unPkgName pkgName <> ", reason: "
              <> T.pack (show err) <> ".")
