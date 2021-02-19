
module Idream.Command.Common ( setupBuildDir
                             , readProjFile
                             , readRootProjFile
                             , readPkgFile
                             , getPkgDirPath
                             , getPkgFilePath
                             , PkgParseErr(..)
                             , ProjParseErr(..)
                             ) where

-- Imports

import Prelude hiding ( readFile )
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Idream.Types ( Project(..), ProjectName(..), Package(..), PackageName(..) )
import Idream.Effects.FileSystem
import Idream.ToText
import System.FilePath ( (</>) )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding  ( encodeUtf8 )
import Data.Aeson ( eitherDecode )


-- Data types

-- | Error type for describing errors when parsing project file.
newtype ProjParseErr = ProjParseErr String
  deriving (Eq, Show)

-- | Error type used for describing errors that can occur while reading out a package file.
newtype PkgParseErr = PkgParseErr String
  deriving (Eq, Show)


-- Instances

instance ToText ProjParseErr where
  toText (ProjParseErr err) =
    "Failed to parse project file: " <> toText err <> "."

instance ToText PkgParseErr where
  toText (PkgParseErr err) =
    "Failed to parse package file: " <> toText err <> "."


-- Functions

-- | Creates a build directory in which idream will store all build artifacts.
setupBuildDir :: Member FileSystem r => Eff r ()
setupBuildDir = createDir buildDir

-- | Reads out a project file (idr-project.json).

readProjFile :: ( Member (Error ProjParseErr) r, Member FileSystem r )
             => FilePath -> Eff r Project
readProjFile file = do
  projectJSON <- encodeUtf8 . TL.fromStrict <$> readFile file
  either (throwError . ProjParseErr) return $ eitherDecode projectJSON

-- | Reads out the top level project file (idr-project.json).
readRootProjFile :: ( Member (Error ProjParseErr) r, Member FileSystem r )
                 => Eff r Project
readRootProjFile = readProjFile projectFile

-- | Reads out a package file (idr-package.json)
readPkgFile :: ( Member (Error PkgParseErr) r, Member FileSystem r )
            => FilePath -> Eff r Package
readPkgFile file = do
  pkgJSON <- encodeUtf8 . TL.fromStrict <$> readFile file
  either (throwError . PkgParseErr) return $ eitherDecode pkgJSON

-- Helper function to determine location of package directory.
getPkgDirPath :: ( Member (Error ProjParseErr) r, Member FileSystem r )
              => PackageName -> ProjectName
              -> Eff r Directory
getPkgDirPath pkg@(PackageName pkgName) (ProjectName projName) = do
  (Project _ rootPkgNames) <- readRootProjFile
  let basePath = if pkg `elem` rootPkgNames
                   then "."
                   else buildDir </> "src" </> T.unpack projName
  return $ basePath </> T.unpack pkgName

-- Helper function to determine location of package file.
getPkgFilePath :: ( Member (Error ProjParseErr) r, Member FileSystem r )
               => PackageName -> ProjectName
               -> Eff r FilePath
getPkgFilePath pkgName projName =
  (</> pkgFile) <$> getPkgDirPath pkgName projName

