
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Idream.Command.New ( startNewProject ) where


-- Imports

import Control.Monad.Reader
import Control.Monad.Except
import Idream.Log ( MonadLogger )
import qualified Idream.Log as Log
import Control.Exception ( IOException )
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import qualified Data.Text as T
import System.FilePath ( (</>) )
import Idream.Types ( Config(..), Directory
                    , BuildSettings(..), ProjectName(..) )
import Idream.Command.Common ( safeWriteFile, safeCreateDir )


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
startNewProject :: ( MonadReader Config m, MonadLogger m, MonadIO m )
                => ProjectName -> m ()
startNewProject projName =
  either showError return =<< runExceptT (startNewProject' projName)

-- | Does the actual creation of the project template.
startNewProject' :: ( MonadError MkProjectError m
                    , MonadReader Config m
                    , MonadIO m)
                 => ProjectName -> m ()
startNewProject' (ProjectName projName) = do
  buildCfg <- asks buildSettings
  safeCreateDir' projectDir
  safeCreateDir' $ projectDir </> buildDir buildCfg
  safeWriteFile' gitignore $ relPath ".gitignore"
  safeWriteFile' (idrProjectJson projName) (relPath $ projectFile buildCfg)
  safeWriteFile' idrPkgSetJson $ relPath (pkgSetFile buildCfg)
  liftIO $ print ("Successfully initialized project: " <> projName <> ".")
  where projectDir = T.unpack projName
        relPath path = projectDir </> path

-- | Displays the error if one occurred during project template creation.
showError :: MonadLogger m => MkProjectError -> m ()
showError (MkDirError e) = Log.err ("Failed to initialize project: " <> T.pack (show e))
showError (MkFileError e) = Log.err ("Failed to initialize project: " <> T.pack (show e))

-- | Safely creates a directory, while handling possible exceptions.
safeCreateDir' :: ( MonadError MkProjectError m
                  , MonadIO m)
               => Directory -> m ()
safeCreateDir' = safeCreateDir MkDirError

-- | Safely writes to a file, while handling possible exceptions.
safeWriteFile' :: (MonadError MkProjectError m, MonadIO m)
               => Text -> FilePath -> m ()
safeWriteFile' = safeWriteFile MkFileError

