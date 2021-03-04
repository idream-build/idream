module Idream.Command.Add
  ( addImpl
  , PackageDirAlreadyExistsErr (..)
  , PackageNameAlreadyExistsErr (..)
  ) where

import qualified Data.Text as T
import Idream.Command.Common (readProjFile, resolveProj)
import Idream.Effects.FileSystem (fsCreateDir, fsDoesDirectoryExist, fsWriteFile)
import Idream.Effects.Serde (serdeWriteJSON)
import Idream.FileLogic (pkgFileName, projFileName)
import Idream.Prelude
import Idream.Types.Common (PackageName, PackageType (..))
import Idream.Types.External (Package (..), Project (..))
import Idream.Types.Internal (LocatedPackage (..), ResolvedProject (..))

data PackageDirAlreadyExistsErr = PackageDirAlreadyExistsErr Directory PackageName
  deriving (Eq, Show)

instance Exception PackageDirAlreadyExistsErr where
  displayException (PackageDirAlreadyExistsErr pkgDir pn) =
    "Failed to add package " <> toString pn <> " to project; directory " <> pkgDir <> " already exists."

data PackageNameAlreadyExistsErr = PackageNameAlreadyExistsErr Directory PackageName
  deriving (Eq, Show)

instance Exception PackageNameAlreadyExistsErr where
  displayException (PackageNameAlreadyExistsErr pkgDir pn) =
    "Failed to add package " <> toString pn <> " to project; it already exists at " <> pkgDir

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
idrPkgJsonContents pn pkgType =
  T.unlines
    [ "{"
    , "  \"name\": \"" <> toText pn <> "\","
    , "  \"type\": \"" <> toText pkgType <> "\","
    , "  \"sourcedir\": \"src\","
    , "  \"depends\": ["
    , "  ]"
    , "}"
    ]

mainFile :: PackageType -> FilePath
mainFile PkgTypeLibrary = "Lib.idr"
mainFile _ = "Main.idr"

mainContents :: PackageType -> Text
mainContents PkgTypeLibrary = libIdrContents
mainContents _ = mainIdrContents

-- | Creates a new project template.
addImpl :: Directory -> Maybe Directory -> PackageName -> PackageType -> AppM ()
addImpl projDir mayPkgSubDir pkgName pkgType = do
  let pkgSubDir = fromMaybe (toString pkgName) mayPkgSubDir
      pkgDir = projDir </> pkgSubDir
  pkgDirExists <- fsDoesDirectoryExist pkgDir
  if pkgDirExists
    then throwIO (PackageDirAlreadyExistsErr pkgDir pkgName)
    else do
      let pkgSrcDir = pkgDir </> "src"
          projFile = projDir </> projFileName
      proj <- readProjFile projFile
      resolvedProj <- resolveProj projDir proj
      for_ (rpPackages resolvedProj) $ \(LocatedPackage d p) ->
        when (packageName p == pkgName) (throwIO (PackageNameAlreadyExistsErr d pkgName))
      fsCreateDir pkgDir
      fsCreateDir pkgSrcDir
      fsWriteFile (pkgDir </> pkgFileName) (idrPkgJsonContents pkgName pkgType)
      fsWriteFile (pkgSrcDir </> mainFile pkgType) (mainContents pkgType)
      let paths = fromMaybe [] (projectPaths proj) ++ [pkgSubDir]
          proj' = proj { projectPaths = Just paths }
      serdeWriteJSON projFile proj'
      logInfo ("Successfully added package " <> toText pkgName <> " to project.")
