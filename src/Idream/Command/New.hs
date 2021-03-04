module Idream.Command.New
  ( newImpl
  , ProjectDirAlreadyExistsErr (..)
  ) where

import qualified Data.Text as T
import Idream.Effects.FileSystem (fsCreateDir, fsDoesDirectoryExist, fsWriteFile)
import Idream.FileLogic (buildDir, pkgSetFileName, projFileName)
import Idream.Prelude
import Idream.Types.Common (ProjectName)

data ProjectDirAlreadyExistsErr = ProjectDirAlreadyExistsErr Directory ProjectName
  deriving (Eq, Show)

instance Exception ProjectDirAlreadyExistsErr where
  displayException (ProjectDirAlreadyExistsErr projDir pn) =
    "Failed to add package " <> toString pn <> " to project; directory " <> projDir <> " already exists."

gitignoreContents, idrPkgSetJsonContents :: Text
gitignoreContents = "/" <> toText buildDir <> "\n"
idrPkgSetJsonContents = T.unlines
  [ "{"
  , "  \"repos\": {"
  , "  },"
  , "  \"packages\": {"
  , "  },"
  , "  \"projects\": ["
  , "  ]"
  , "}"
  ]

idrProjectJsonContents :: ProjectName -> Text
idrProjectJsonContents jn = T.unlines
  [ "{"
  , "  \"name\": \"" <> toText jn <> "\","
  , "  \"paths\": ["
  , "  ]"
  , "}"
  , ""
  ]

-- | Creates a new project template.
newImpl :: Directory -> ProjectName -> AppM ()
newImpl projDir projName = do
  -- TODO(ejconlon) Allow this if dir is empty?
  exists <- fsDoesDirectoryExist projDir
  when exists (throwIO (ProjectDirAlreadyExistsErr projDir projName))
  fsCreateDir projDir
  fsCreateDir (relPath buildDir)
  fsWriteFile (relPath ".gitignore") gitignoreContents
  fsWriteFile (relPath projFileName) (idrProjectJsonContents projName)
  fsWriteFile (relPath pkgSetFileName) idrPkgSetJsonContents
  logInfo ("Successfully initialized project: " <> toText projName)
  where relPath path = projDir </> path
