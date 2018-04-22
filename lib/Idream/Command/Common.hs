
module Idream.Command.Common ( setupBuildDir
                             , readProjFile
                             , readRootProjFile
                             , readPkgFile
                             , getPkgDirPath
                             , getPkgFilePath
                             , handleReadProjectErr
                             , handleReadPkgErr
                             , PkgParseErr(..)
                             , ProjParseErr(..)
                             ) where

-- Imports

import Prelude hiding ( readFile )
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Idream.Types ( Project(..), ProjectName(..), Package(..), PackageName(..) )
import Idream.Effects.FileSystem
import System.FilePath ( (</>) )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding  ( encodeUtf8 )
import Data.Aeson ( eitherDecode )
import Data.Monoid ( (<>) )


-- Data types

-- | Error type for describing errors when parsing project file.
newtype ProjParseErr = ProjParseErr String
  deriving (Eq, Show)

-- | Error type used for describing errors that can occur while reading out a package file.
newtype PkgParseErr = PkgParseErr String
  deriving (Eq, Show)


-- Functions

-- | Creates a build directory in which idream will store all build artifacts.
setupBuildDir :: Member FileSystem r => Eff r ()
setupBuildDir = createDir buildDir

-- | Reads out a project file (idr-project.json).

readProjFile :: ( Member (Error e) r, Member FileSystem r )
             => (ProjParseErr -> e)
             -> FilePath -> Eff r Project
readProjFile f file = do
  projectJSON <- encodeUtf8 . TL.fromStrict <$> readFile file
  either (throwError . f . ProjParseErr) return $ eitherDecode projectJSON

-- | Reads out the top level project file (idr-project.json).
readRootProjFile :: ( Member (Error e) r, Member FileSystem r )
                 => (ProjParseErr -> e)
                 -> Eff r Project
readRootProjFile f = readProjFile f projectFile

-- | Reads out a package file (idr-package.json)
readPkgFile :: ( Member (Error e) r, Member FileSystem r )
            => (PkgParseErr -> e)
            -> FilePath -> Eff r Package
readPkgFile f file = do
  pkgJSON <- encodeUtf8 . TL.fromStrict <$> readFile file
  either (throwError . f . PkgParseErr) return $ eitherDecode pkgJSON

-- Helper function to determine location of package directory.
getPkgDirPath :: ( Member (Error e) r, Member FileSystem r )
              => (ProjParseErr -> e)
              -> PackageName -> ProjectName
              -> Eff r Directory
getPkgDirPath f pkg@(PackageName pkgName) (ProjectName projName) = do
  (Project _ rootPkgNames) <- readRootProjFile f
  let basePath = if pkg `elem` rootPkgNames
                   then "."
                   else buildDir </> "src" </> T.unpack projName
  return $ basePath </> T.unpack pkgName

-- Helper function to determine location of package file.
getPkgFilePath :: ( Member (Error e) r, Member FileSystem r )
               => (ProjParseErr -> e)
               -> PackageName -> ProjectName
               -> Eff r FilePath
getPkgFilePath f pkgName projName =
  (</> pkgFile) <$> getPkgDirPath f pkgName projName

-- | Helper function for handling errors related to
--   readout of project file.
handleReadProjectErr :: ProjParseErr -> T.Text
handleReadProjectErr (ProjParseErr err) =
  T.pack $ "Failed to parse project file: " <> err <> "."

-- | Helper function for handling errors related to
--   readout of package file.
handleReadPkgErr :: PkgParseErr -> T.Text
handleReadPkgErr (PkgParseErr err) =
  T.pack $ "Failed to parse package file: " <> err <> "."

