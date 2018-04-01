
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Add ( addPackageToProject ) where


-- Imports

import Control.Monad.Reader
import qualified Idream.Log as Log
import Idream.Log ( MonadLogger )
import Idream.SafeIO
import Control.Exception ( IOException )
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import Data.Text.Lazy.Encoding ( decodeUtf8 )
import Data.Aeson.Encode.Pretty ( encodePretty )
import Data.Aeson.Types ( ToJSON(..) )
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import System.Directory ( doesDirectoryExist )
import System.FilePath ( (</>) )
import Idream.Types ( Config(..), Directory, BuildSettings(..)
                    , Project(..), PackageName(..), PackageType(..) )
import Idream.Command.Common ( safeWriteFile, safeCreateDir
                             , readProjFile, handleReadProjectErr
                             , ReadProjectErr(..) )


-- Data types

-- | Custom error type for when creating a project template.
data AddPackageError = PackageAlreadyExistsErr PackageName
                     | ReadProjFileErr ReadProjectErr
                     | MkDirError IOException
                     | MkFileError IOException
                     deriving (Eq, Show)


-- Functions

libIdr, mainIdr :: Text
idrPkgJson :: Text -> PackageType -> Text

libIdr =
  T.unlines [ "module Lib"
            , ""
            , "||| Library function, to be replaced with actual code."
            , "libFunction : String"
            , "libFunction = \"Hello, Idris!\""
            ]
mainIdr =
  T.unlines [ "module Main"
            , ""
            , "||| Main program, to be replaced with actual code."
            , "main : IO ()"
            , "main = putStrLn \"Hello, Idris!\""
            ]
idrPkgJson pkgName pkgType =
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
addPackageToProject :: ( MonadReader Config m, MonadLogger m, MonadIO m )
                    => PackageName -> PackageType -> m ()
addPackageToProject pkgName@(PackageName name) pkgType = do
  pkgDirExists <- liftIO $ doesDirectoryExist (T.unpack name)
  if pkgDirExists
    then showError $ PackageAlreadyExistsErr pkgName
    else do
      result <- runSafeIO $ addPackageToProject' pkgName pkgType
      either showError return result

-- | Does the actual creation of the project template.
addPackageToProject' :: ( MonadReader Config m
                        , MonadLogger m
                        , MonadSafeIO AddPackageError m )
                     => PackageName -> PackageType -> m ()
addPackageToProject' pkgName@(PackageName name) pkgType = do
  buildCfg <- asks buildSettings
  projInfo <- readProjInfo buildCfg
  safeCreateDir' pkgDir
  safeCreateDir' srcDir
  safeWriteFile' (idrPkgJson name pkgType) (pkgDir </> pkgFile buildCfg)
  safeWriteFile' (mainContents pkgType) (srcDir </> mainFile pkgType)
  updateProjInfo buildCfg projInfo pkgName
  displayStatusUpdate pkgName
  where pkgDir = T.unpack name
        srcDir = pkgDir </> "src"
        mainFile Library = "Lib.idr"
        mainFile Executable = "Main.idr"
        mainContents Library = libIdr
        mainContents Executable = mainIdr


-- | Tries to read the project file.
readProjInfo :: MonadSafeIO AddPackageError m => BuildSettings -> m Project
readProjInfo buildCfg = readProjFile ReadProjFileErr $ projectFile buildCfg

-- | Updates the project file with a new package entry.
updateProjInfo :: MonadSafeIO AddPackageError m
               => BuildSettings -> Project -> PackageName -> m ()
updateProjInfo buildCfg projInfo pkgName =
  let projFilePath = projectFile buildCfg
      updatedDeps = projDeps projInfo ++ [pkgName]
      projInfo' = projInfo { projDeps = updatedDeps }
   in safeWriteFile' (encodeJSON projInfo') projFilePath

displayStatusUpdate :: MonadLogger m => PackageName -> m ()
displayStatusUpdate (PackageName pkgName) =
  Log.info ("Successfully added package " <> pkgName <> " to project.")

-- | Displays the error if one occurred during project template creation.
showError :: MonadLogger m => AddPackageError -> m ()
showError (ReadProjFileErr err) = handleReadProjectErr err
showError (PackageAlreadyExistsErr (PackageName pkgName)) =
  Log.err ("Failed to add package to project, package "
          <> pkgName <> " already exists")
showError (MkDirError e) =
  Log.err ("Failed to add package to project: " <> T.pack (show e))
showError (MkFileError e) =
  Log.err ("Failed to add package to project: " <> T.pack (show e))


-- | Safely creates a directory while handling possible exceptions.
safeCreateDir' :: MonadSafeIO AddPackageError m => Directory -> m ()
safeCreateDir' = safeCreateDir MkDirError

-- | Safely writes to a file, while handling possible exceptions.
safeWriteFile' :: MonadSafeIO AddPackageError m => Text -> FilePath -> m ()
safeWriteFile' = safeWriteFile MkFileError

encodeJSON :: ToJSON a => a -> Text
encodeJSON x = (TL.toStrict . decodeUtf8 . encodePretty $ x) <> "\n"

