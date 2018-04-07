
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Idream.Command.New ( startNewProject ) where


-- Imports

import Idream.Log ( MonadLogger )
import qualified Idream.Log as Log
import Idream.SafeIO ( MonadSafeIO, MonadIO, runSafeIO )
import Control.Exception ( IOException )
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import qualified Data.Text as T
import Idream.Types ( ProjectName(..) )
import Idream.Command.Common ( safeWriteFile, safeCreateDir )
import Idream.FileSystem


-- Data types

-- | Custom error type for when creating a project template.
data MkProjectError = MkDirError IOException
                    | MkFileError IOException
                    deriving (Eq, Show)


-- Functions

gitignore, idrPkgSetJson :: Text
idrProjectJson :: Text -> Text

gitignore = ".idream-work/\n"
idrPkgSetJson = "{}\n"
idrProjectJson projName =
  T.unlines [ "{"
            , "    \"project_name\": \"" <> projName <> "\","
            , "    \"packages\": ["
            , ""
            , "    ]"
            , "}"
            , ""
            ]


-- | Creates a new project template.
startNewProject :: ( MonadLogger m, MonadIO m )
                => ProjectName -> m ()
startNewProject projName = do
  result <- runSafeIO (startNewProject' projName)
  either showError return result


-- | Does the actual creation of the project template.
startNewProject' :: ( MonadLogger m, MonadSafeIO MkProjectError m )
                 => ProjectName -> m ()
startNewProject' (ProjectName projName) = do
  safeCreateDir' projectDir
  safeCreateDir' $ projectDir </> buildDir
  safeWriteFile' gitignore $ relPath ".gitignore"
  safeWriteFile' (idrProjectJson projName) (relPath projectFile)
  safeWriteFile' idrPkgSetJson $ relPath pkgSetFile
  Log.info ("Successfully initialized project: " <> projName <> ".")
  where projectDir = T.unpack projName
        relPath path = projectDir </> path

-- | Displays the error if one occurred during project template creation.
showError :: MonadLogger m => MkProjectError -> m ()
showError (MkDirError e) = Log.err ("Failed to initialize project: " <> T.pack (show e))
showError (MkFileError e) = Log.err ("Failed to initialize project: " <> T.pack (show e))

-- | Safely creates a directory, while handling possible exceptions.
safeCreateDir' :: MonadSafeIO MkProjectError m => Directory -> m ()
safeCreateDir' = safeCreateDir MkDirError

-- | Safely writes to a file, while handling possible exceptions.
safeWriteFile' :: MonadSafeIO MkProjectError m => Text -> FilePath -> m ()
safeWriteFile' = safeWriteFile MkFileError

