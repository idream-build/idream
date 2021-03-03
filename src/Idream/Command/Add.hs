module Idream.Command.Add
  ( addPackageToProject
  , PackageDirAlreadyExistsErr (..)
  , PackageNameAlreadyExistsErr (..)
  ) where

import Control.Exception (Exception (..))
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Idream.App (AppM)
import Idream.Command.Common (readProjFile, resolveProj)
import Idream.Effects.FileSystem (fsCreateDir, fsDoesDirectoryExist, fsWriteFile)
import Idream.Effects.Serde (serdeWriteJSON)
import Idream.FileLogic (pkgFileName, projFileName)
import Idream.FilePaths (Directory)
import Idream.Types.Common (PackageName (..), PackageType (..))
import Idream.Types.External (Package (..), Project (..))
import Idream.Types.Internal (ResolvedProject (..), LocatedPackage (..))
import LittleLogger (logInfo)
import System.FilePath ((</>))
import UnliftIO.Exception (throwIO)

data PackageDirAlreadyExistsErr = PackageDirAlreadyExistsErr Directory PackageName
  deriving (Eq, Show)

instance Exception PackageDirAlreadyExistsErr where
  displayException (PackageDirAlreadyExistsErr pkgDir (PackageName p)) =
    "Failed to add package " <> T.unpack p <> " to project; directory " <> pkgDir <> " already exists."

data PackageNameAlreadyExistsErr = PackageNameAlreadyExistsErr Directory PackageName
  deriving (Eq, Show)

instance Exception PackageNameAlreadyExistsErr where
  displayException (PackageNameAlreadyExistsErr pkgDir (PackageName p)) =
    "Failed to add package " <> T.unpack p <> " to project; it already exists at " <> pkgDir

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
  T.unlines
    [ "{"
    , "  \"name\": \"" <> pkgName <> "\","
    , "  \"source_dir\": \"src\","
    , if pkgType == PkgTypeLibrary
      then "  \"executable\": false,"
      else "  \"executable\": true,"
    , "  \"dependencies\": []"
    , "}"
    ]

mainFile :: PackageType -> FilePath
mainFile PkgTypeLibrary = "Lib.idr"
mainFile _ = "Main.idr"

mainContents :: PackageType -> Text
mainContents PkgTypeLibrary = libIdrContents
mainContents _ = mainIdrContents

-- | Creates a new project template.
addPackageToProject :: Directory -> Directory -> PackageName -> PackageType -> AppM ()
addPackageToProject projDir pkgSubDir pkgName pkgType = do
  let pkgDir = projDir </> pkgSubDir
  pkgDirExists <- fsDoesDirectoryExist pkgDir
  if pkgDirExists
    then throwIO (PackageDirAlreadyExistsErr pkgDir pkgName)
    else do
      let pkgSrcDir = pkgDir </> "src"
          projFile = projDir </> projFileName
      proj <- readProjFile projFile
      resolvedProj <- resolveProj proj
      for_ (rpPackages resolvedProj) $ \(LocatedPackage d p) ->
        when (packageName p == pkgName) (throwIO (PackageNameAlreadyExistsErr d pkgName))
      fsCreateDir pkgDir
      fsCreateDir pkgSrcDir
      fsWriteFile (pkgDir </> pkgFileName) (idrPkgJsonContents pkgName pkgType)
      fsWriteFile (pkgSrcDir </> mainFile pkgType) (mainContents pkgType)
      let paths = fromMaybe [] (projectPaths proj) ++ [pkgSubDir]
          proj' = proj { projectPaths = Just paths }
      serdeWriteJSON projFile proj'
      logInfo ("Successfully added package " <> unPkgName pkgName <> " to project.")
