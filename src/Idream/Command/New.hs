module Idream.Command.New
  ( startNewProject
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Idream.App (AppM, appCreateDir, appWriteFile)
import Idream.FilePaths (buildDir, pkgSetFile, projectFile)
import Idream.ToText (ToText (..))
import Idream.Types (ProjectName (..))
import LittleLogger (logInfo)
import System.FilePath ((</>))

gitignoreContents, idrPkgSetJsonContents :: Text
gitignoreContents = ".idream-work/\n"
idrPkgSetJsonContents = "{}\n"

idrProjectJsonContents :: ProjectName -> Text
idrProjectJsonContents projName =
  T.unlines [ "{"
            , "    \"project_name\": \"" <> toText projName <> "\","
            , "    \"packages\": ["
            , ""
            , "    ]"
            , "}"
            , ""
            ]

-- | Creates a new project template.
startNewProject :: ProjectName -> AppM ()
startNewProject projName = do
  appCreateDir projectDir
  appCreateDir (relPath buildDir)
  appWriteFile (relPath ".gitignore") gitignoreContents
  appWriteFile (relPath projectFile) (idrProjectJsonContents projName)
  appWriteFile (relPath pkgSetFile) idrPkgSetJsonContents
  logInfo ("Successfully initialized project: " <> toText projName <> ".")
  where projectDir = T.unpack (toText projName)
        relPath path = projectDir </> path
