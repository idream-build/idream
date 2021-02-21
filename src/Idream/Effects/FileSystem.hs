module Idream.Effects.FileSystem
  ( fsCopyDir
  , fsCreateDir
  , fsDoesDirectoryExist
  , fsDoesFileExist
  , fsFindFiles
  , fsRemovePath
  , fsWriteFile
  , WriteFileErr (..)
  , ReadFileErr (..)
  , CreateDirErr (..)
  ) where

import Control.Exception (Exception (..), IOException)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Idream.App (AppM)
import Idream.FilePaths (Directory)
import UnliftIO.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import UnliftIO.Exception (catchIO, throwIO)

data WriteFileErr = WriteFileErr FilePath IOException
  deriving (Eq, Show)

instance Exception WriteFileErr where
  displayException (WriteFileErr path err) =
    "Failed to write to file (" <> path <> "), reason: " <> displayException err <> "."

data ReadFileErr = ReadFileErr FilePath IOException
  deriving (Eq, Show)

instance Exception ReadFileErr where
  displayException (ReadFileErr path err) =
    "Failed to read from file (" <> path <> "), reason: " <> displayException err <> "."

data CreateDirErr = CreateDirErr Directory IOException
  deriving (Eq, Show)

instance Exception CreateDirErr where
  displayException (CreateDirErr dir err) =
    "Failed to create directory (" <> dir <> "), reason: " <> displayException err <> "."

fsCopyDir :: Directory -> Directory -> AppM ()
fsCopyDir _fromDir _toDir = error "TODO: copy dir"

fsCreateDir :: Directory -> AppM ()
fsCreateDir dir = catchIO (createDirectoryIfMissing False dir) (throwIO . CreateDirErr dir)

fsDoesDirectoryExist :: Directory -> AppM Bool
fsDoesDirectoryExist = doesDirectoryExist

fsDoesFileExist :: FilePath -> AppM Bool
fsDoesFileExist = doesFileExist

fsFindFiles :: (FilePath -> Bool) -> Maybe Directory -> AppM [FilePath]
fsFindFiles _pcate _mayDir = error "TODO: find files"

fsRemovePath :: FilePath -> AppM ()
fsRemovePath _path = error "TODO: remove path"

fsWriteFile :: FilePath -> Text -> AppM ()
fsWriteFile path txt = catchIO (liftIO (TIO.writeFile path txt)) (throwIO . WriteFileErr path)
