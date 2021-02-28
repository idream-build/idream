module Idream.Command.New
  ( startNewProject
  , ProjectDirAlreadyExistsErr (..)
  ) where

import Control.Exception (Exception (..))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Idream.App (AppM)
import Idream.Effects.FileSystem (fsCreateDir, fsDoesDirectoryExist, fsWriteFile)
import Idream.FileLogic (buildDir, projFileName, pkgSetFileName)
import Idream.FilePaths (Directory)
import Idream.Types.Common (ProjectName (..))
import LittleLogger (logInfo)
import System.FilePath ((</>))
import UnliftIO.Exception (throwIO)

data ProjectDirAlreadyExistsErr = ProjectDirAlreadyExistsErr Directory ProjectName
  deriving (Eq, Show)

instance Exception ProjectDirAlreadyExistsErr where
  displayException (ProjectDirAlreadyExistsErr projDir (ProjectName pn)) =
    "Failed to add package " <> T.unpack pn <> " to project; directory " <> projDir <> " already exists."

gitignoreContents, idrPkgSetJsonContents :: Text
gitignoreContents = "/" <> T.pack buildDir <> "\n"
idrPkgSetJsonContents = "{}\n"

idrProjectJsonContents :: ProjectName -> Text
idrProjectJsonContents (ProjectName pn) =
  T.unlines [ "{"
            , "    \"name\": \"" <> pn <> "\","
            , "    \"paths\": ["
            , ""
            , "    ]"
            , "}"
            , ""
            ]

-- | Creates a new project template.
startNewProject :: Directory -> ProjectName -> AppM ()
startNewProject projDir projName = do
  exists <- fsDoesDirectoryExist projDir
  when exists (throwIO (ProjectDirAlreadyExistsErr projDir projName))
  fsCreateDir projDir
  fsCreateDir (relPath buildDir)
  fsWriteFile (relPath ".gitignore") gitignoreContents
  fsWriteFile (relPath projFileName) (idrProjectJsonContents projName)
  fsWriteFile (relPath pkgSetFileName) idrPkgSetJsonContents
  logInfo ("Successfully initialized project: " <> unProjName projName <> ".")
  where relPath path = projDir </> path
