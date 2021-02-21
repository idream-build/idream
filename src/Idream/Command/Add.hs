module Idream.Command.Add
  ( addPackageToProject
  , PackageAlreadyExistsErr (..)
  ) where

import Control.Exception (Exception (..))
import Data.Text (Text)
import qualified Data.Text as T
import Idream.App (AppM)
import Idream.Command.Common (readRootProjFile)
import Idream.Effects.FileSystem (fsCreateDir, fsDoesDirectoryExist, fsWriteFile)
import Idream.Effects.Serde (serdeWriteJSON)
import Idream.FileLogic (pkgDir, pkgFile, pkgSrcDir, projectFile)
import Idream.ToText (ToText (..))
import Idream.Types (PackageName (..), PackageType (..), Project (..))
import LittleLogger (logInfo)
import System.FilePath ((</>))
import UnliftIO.Exception (throwIO)

newtype PackageAlreadyExistsErr = PackageAlreadyExistsErr PackageName
  deriving (Eq, Show)

instance Exception PackageAlreadyExistsErr where
  displayException (PackageAlreadyExistsErr (PackageName p)) =
    "Failed to add package to project, package " <> T.unpack p <> " already exists."

libIdrContents, mainIdrContents :: Text
libIdrContents =
  T.unlines [ "module Lib"
            , ""
            , "||| Library function, to be replaced with actual code."
            , "libFunction : String"
            , "libFunction = \"Hello, Idris!\""
            ]
mainIdrContents =
  T.unlines [ "module Main"
            , ""
            , "||| Main program, to be replaced with actual code."
            , "main : IO ()"
            , "main = putStrLn \"Hello, Idris!\""
            ]

idrPkgJsonContents :: PackageName -> PackageType -> Text
idrPkgJsonContents (PackageName pkgName) pkgType =
  T.unlines [ "{"
            , "    \"name\": \"" <> pkgName <> "\","
            , "    \"source_dir\": \"src\","
            , if pkgType == Library
              then "    \"executable\": false,"
              else "    \"executable\": true,"
            , "    \"dependencies\": []"
            , "}"
            ]

-- | Creates a new project template.
addPackageToProject :: PackageName -> PackageType -> AppM ()
addPackageToProject pkgName@(PackageName name) pkgType = do
  pkgDirExists <- fsDoesDirectoryExist (T.unpack name)
  if pkgDirExists
    then throwIO (PackageAlreadyExistsErr pkgName)
    else addPackageToProject' pkgName pkgType

-- | Does the actual creation of the project template.
addPackageToProject' :: PackageName -> PackageType -> AppM ()
addPackageToProject' pkgName pkgType = do
  projInfo <- readRootProjFile
  fsCreateDir (pkgDir pkgName)
  fsCreateDir (pkgSrcDir pkgName)
  fsWriteFile (pkgDir pkgName </> pkgFile) (idrPkgJsonContents pkgName pkgType)
  fsWriteFile (pkgSrcDir pkgName </> mainFile pkgType) (mainContents pkgType)
  updateProjInfo projInfo pkgName
  logInfo ("Successfully added package " <> toText pkgName <> " to project.")
  where mainFile Library = "Lib.idr"
        mainFile Executable = "Main.idr"
        mainContents Library = libIdrContents
        mainContents Executable = mainIdrContents

-- | Updates the project file with a new package entry.
updateProjInfo :: Project -> PackageName -> AppM ()
updateProjInfo projInfo pkgName =
  let projFilePath = projectFile
      updatedDeps = projDeps projInfo ++ [pkgName]
      projInfo' = projInfo { projDeps = updatedDeps }
   in serdeWriteJSON projFilePath projInfo'
