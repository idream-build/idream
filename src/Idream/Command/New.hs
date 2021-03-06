module Idream.Command.New
  ( newImpl
  , ProjectDirAlreadyExistsErr (..)
  ) where

import qualified Data.Text as T
import Idream.Effects.FileSystem (fsCreateDir, fsDoesDirectoryExist, fsWriteFile)
import Idream.FileLogic (pkgSetFileName, projFileName, workDir)
import Idream.Prelude
import Idream.Types.Common (ProjectName)

data ProjectDirAlreadyExistsErr = ProjectDirAlreadyExistsErr Directory ProjectName
  deriving (Eq, Show)

instance Exception ProjectDirAlreadyExistsErr where
  displayException (ProjectDirAlreadyExistsErr projDir pn) =
    "Failed to add package " <> toString pn <> " to project; directory " <> projDir <> " already exists."

gitignoreContents, idrPkgSetJsonContents :: Text
gitignoreContents = "/" <> toText workDir <> "\n"
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
newImpl :: Directory -> ProjectName -> Bool -> AppM ()
newImpl projDir projName allowExisting = do
  exists <- fsDoesDirectoryExist projDir
  when (exists && not allowExisting) (throwIO (ProjectDirAlreadyExistsErr projDir projName))
  fsCreateDir projDir
  fsWriteFile (projDir </> ".gitignore") gitignoreContents
  fsWriteFile (projDir </> projFileName) (idrProjectJsonContents projName)
  fsWriteFile (projDir </> pkgSetFileName) idrPkgSetJsonContents
  logInfo ("Successfully initialized project: " <> toText projName)
