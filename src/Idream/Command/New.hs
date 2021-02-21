module Idream.Command.New
  ( startNewProject
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Idream.App (AppM)
import Idream.Effects.FileSystem (fsCreateDir, fsWriteFile)
import Idream.FileLogic (buildDir, pkgSetFile, projectFile)
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
  fsCreateDir projectDir
  fsCreateDir (relPath buildDir)
  fsWriteFile (relPath ".gitignore") gitignoreContents
  fsWriteFile (relPath projectFile) (idrProjectJsonContents projName)
  fsWriteFile (relPath pkgSetFile) idrPkgSetJsonContents
  logInfo ("Successfully initialized project: " <> toText projName <> ".")
  where projectDir = T.unpack (toText projName)
        relPath path = projectDir </> path
