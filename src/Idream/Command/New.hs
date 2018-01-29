
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.New ( startNewProject ) where


-- Imports

import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Except
import Control.Exception ( IOException )
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory ( createDirectory )
import System.FilePath ( (</>) )
import Idream.Types ( Config(..)
                    , PackageName(..)
                    , PackageType(..) )
import Idream.Command.Common ( tryAction )


-- Data types

type Directory = FilePath
-- | Custom error type for when creating a project template.
data MkProjectError = MkDirError IOException
                    | MkFileError IOException
                    deriving (Eq, Show)


-- Functions

gitignore, idreamJson, idrPkgSetJson, libIdr, mainIdr :: Text
idrProjectJson :: Text -> Text
idrPkgJson :: Text -> PackageType -> Text

gitignore = ".idream-work/"
idreamJson =
  T.unlines [ "{"
            , "    \"cache-dir\": \".idream-work/\","
            , "    \"project-file\": \"idr-project.json\""
            , "}"
            ]
idrPkgJson pkgName pkgType =
  T.unlines [ "{"
            , "    \"name\": \"" <> pkgName <> "\","
            , "    \"sourcedir\": \"src\","
            , if pkgType == Library
              then "    \"executable\": false,"
              else "    \"executable\": true,"
            , "    \"dependencies\": []"
            , "}"
            ]
idrProjectJson pkgName =
  T.unlines [ "{"
            , "    \"packages\": ["
            , "        \"" <> pkgName <> "\""
            , "    ]"
            , "}"
            ]
idrPkgSetJson = "{}"
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

-- | Creates a new project template.
startNewProject :: (MonadReader Config m, MonadLogger m, MonadIO m)
                => PackageName -> PackageType -> m ()
startNewProject pkgName pkgType =
  either showError return =<< runExceptT (startNewProject' pkgName pkgType)

-- | Displays the error if one occurred during project template creation.
showError :: MonadLogger m => MkProjectError -> m ()
showError (MkDirError e) = $(logError) ("Failed to initialize project: " <> T.pack (show e))
showError (MkFileError e) = $(logError) ("Failed to initialize project: " <> T.pack (show e))

-- | Does the actual creation of the project template.
startNewProject' :: (MonadError MkProjectError m, MonadIO m)
                 => PackageName -> PackageType -> m ()
startNewProject' (PackageName pkgName) pkgType = do
  safeCreateDir projectDir
  safeCreateDir $ relDir ".idream-work"
  safeWriteFile gitignore $ relPath ".gitignore"
  safeWriteFile idreamJson $ relPath ".idream.json"
  safeWriteFile (idrProjectJson pkgName) (relPath "idr-project.json")
  safeWriteFile idrPkgSetJson $ relPath "idr-package-set.json"
  safeCreateDir $ relDir projectDir
  safeWriteFile (idrPkgJson pkgName pkgType) (relPath $ projectDir </> "idr-package.json")
  safeCreateDir (relDir $ projectDir </> "src")
  safeWriteFile (mainContents pkgType) (relPath $ projectDir </> "src" </> mainFile pkgType)
  where projectDir = T.unpack pkgName
        relPath path = projectDir </> path
        relDir = relPath
        mainFile Library = "Lib.idr"
        mainFile Executable = "Main.idr"
        mainContents Library = libIdr
        mainContents Executable = mainIdr

-- | Safely creates a directory, while handling possible exceptions.
safeCreateDir :: (MonadError MkProjectError m,
                  MonadIO m)
              => Directory -> m ()
safeCreateDir dir =
  tryAction MkDirError $ createDirectory dir

-- | Safely writes to a file, while handling possible exceptions.
safeWriteFile :: (MonadError MkProjectError m, MonadIO m)
              => Text -> FilePath -> m ()
safeWriteFile txt path =
  tryAction MkFileError $ TIO.writeFile path txt
