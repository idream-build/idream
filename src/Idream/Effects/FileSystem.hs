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
  , CopyDirErr (..)
  , FindFilesErr (..)
  ) where

import Control.Exception (Exception (..), IOException, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Idream.App (AppM)
import Idream.FilePaths (Directory)
import Shelly (cp_r, find, fromText, shelly, silently, toTextIgnore)
import UnliftIO.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removePathForcibly)
import UnliftIO.Exception (catch, catchIO, throwIO)

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

data CopyDirErr = CopyDirErr Directory Directory SomeException
  deriving (Show)

instance Exception CopyDirErr where
  displayException (CopyDirErr fromDir toDir err) =
    "Failed to copy files from " <> fromDir <> " to " <> toDir
      <> ", reason: " <> displayException err <> "."

data FindFilesErr = FindFilesErr Directory SomeException
  deriving (Show)

instance Exception FindFilesErr where
  displayException (FindFilesErr inDir err) =
    "Error when trying to find files (" <> inDir <> "), reason: " <> displayException err <> "."

fsCopyDir :: Directory -> Directory -> AppM ()
fsCopyDir fromDir toDir = catch act (throwIO . CopyDirErr fromDir toDir) where
  toFilePath = fromText . T.pack
  act = shelly (silently (cp_r (toFilePath fromDir) (toFilePath toDir)))

fsCreateDir :: Directory -> AppM ()
fsCreateDir dir = catchIO (createDirectoryIfMissing True dir) (throwIO . CreateDirErr dir)

fsDoesDirectoryExist :: Directory -> AppM Bool
fsDoesDirectoryExist = doesDirectoryExist

fsDoesFileExist :: FilePath -> AppM Bool
fsDoesFileExist = doesFileExist

fsFindFiles :: (FilePath -> Bool) -> Maybe Directory -> AppM [FilePath]
fsFindFiles pcate mayDir = catch act (throwIO . FindFilesErr dir') where
    toFilePath = fromText . T.pack
    fromFilePath = T.unpack . toTextIgnore
    dir' = fromMaybe "." mayDir
    act = do
      files <- shelly (silently (find (toFilePath dir')))
      pure (filter pcate (fmap fromFilePath files))

fsRemovePath :: FilePath -> AppM ()
fsRemovePath = removePathForcibly

fsWriteFile :: FilePath -> Text -> AppM ()
fsWriteFile path txt = catchIO (liftIO (TIO.writeFile path txt)) (throwIO . WriteFileErr path)
