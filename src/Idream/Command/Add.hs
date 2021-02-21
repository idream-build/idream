module Idream.Command.Add
  ( addPackageToProject
  ) where

import Control.Exception (Exception)
import Data.Text (Text)
import qualified Data.Text as T
import Idream.App (AppM, appCreateDir, appDoesDirectoryExist, appLogAndThrow, appWriteFile, appWriteJSON)
import Idream.FilePaths (pkgDir, pkgFile, pkgSrcDir, projectFile)
import Idream.Command.Common (readRootProjFile)
import Idream.ToText (ToText (..))
import Idream.Types (PackageName (..), PackageType (..), Project (..))
import LittleLogger (logInfo)
import System.FilePath ((</>))

data PackageAlreadyExistsErr = PackageAlreadyExistsErr PackageName
  deriving (Eq, Show)

instance Exception PackageAlreadyExistsErr

instance ToText PackageAlreadyExistsErr where
  toText (PackageAlreadyExistsErr pkgName) =
    "Failed to add package to project, package " <> toText pkgName <> " already exists."

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
  pkgDirExists <- appDoesDirectoryExist (T.unpack name)
  if pkgDirExists
    then appLogAndThrow (PackageAlreadyExistsErr pkgName)
    else addPackageToProject' pkgName pkgType

-- | Does the actual creation of the project template.
addPackageToProject' :: PackageName -> PackageType -> AppM ()
addPackageToProject' pkgName pkgType = do
  projInfo <- readRootProjFile
  appCreateDir (pkgDir pkgName)
  appCreateDir (pkgSrcDir pkgName)
  appWriteFile (pkgDir pkgName </> pkgFile) (idrPkgJsonContents pkgName pkgType)
  appWriteFile (pkgSrcDir pkgName </> mainFile pkgType) (mainContents pkgType)
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
   in appWriteJSON projFilePath projInfo'
