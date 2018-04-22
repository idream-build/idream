
{-# LANGUAGE RankNTypes #-}

module Idream.Effects.FileSystem ( FSError(..), FileSystem(..)
                                 , runFS
                                 , writeFile, readFile
                                 , doesFileExist, doesDirExist
                                 , createDir, removePath
                                 , copyDir, findFiles
                                 , module Idream.FilePaths
                                 ) where

-- Imports

import Prelude hiding ( writeFile, readFile )
import qualified System.Directory as Dir
import Idream.FilePaths
import Idream.SafeIO
import Idream.ToText
import Data.Monoid ( (<>) )
import Data.Maybe ( fromMaybe )
import Control.Monad.Freer
import Control.Exception ( IOException )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Shelly ( shelly, silently, cp_r, find
              , fromText, toTextIgnore )


-- Data types

-- | Data type describing possible errors when interacting with the filesystem.
data FSError = WriteFileErr FilePath IOException
             | ReadFileErr FilePath IOException
             | CheckFileExistsErr FilePath IOException
             | CreateDirErr Directory IOException
             | RemovePathErr FilePath IOException
             | CheckDirExistsErr Directory IOException
             | CopyDirErr Directory Directory IOException
             | FindFilesErr Directory IOException
             deriving (Eq, Show)

-- | Data type describing possible interactions with the filesystem.
data FileSystem a where
  WriteFile :: T.Text -> FilePath -> FileSystem ()
  ReadFile :: FilePath -> FileSystem T.Text
  DoesFileExist :: FilePath -> FileSystem Bool
  CreateDir :: Directory -> FileSystem ()
  RemovePath :: FilePath -> FileSystem ()
  DoesDirExist :: Directory -> FileSystem Bool
  CopyDir :: Directory -> Directory -> FileSystem ()
  FindFiles :: (FilePath -> Bool) -> Maybe Directory -> FileSystem [FilePath]


-- Instances

instance ToText FSError where
  toText (WriteFileErr path err) =
    "Failed to write to file (" <> toText path <> "): " <> toText err <> "."
  toText (ReadFileErr path err) =
    "Failed to read from file (" <> toText path <> "): " <> toText err <> "."
  toText (CheckFileExistsErr path err) =
    "Failed to check if file exists (" <> toText path <> "): " <> toText err <> "."
  toText (CreateDirErr dir err) =
    "Failed to create directory (" <> toText dir <> "): " <> toText err <> "."
  toText (RemovePathErr path err) =
    "Failed to remove path (" <> toText path <> "): " <> toText err <> "."
  toText (CheckDirExistsErr dir err) =
    "Failed to check if directory exists (" <> toText dir <> "): "
      <> toText err <> "."
  toText (CopyDirErr from to err) =
    "Failed to copy files from " <> toText from <> " to " <> toText to
      <> ", reason: " <> toText err <> "."
  toText (FindFilesErr dir err) =
    "Error when trying to find files (" <> toText dir <> "): " <> toText err <> "."


-- Functions

writeFile :: Member FileSystem r => T.Text -> FilePath -> Eff r ()
writeFile txt path = send $ WriteFile txt path

readFile :: Member FileSystem r => FilePath -> Eff r T.Text
readFile path = send $ ReadFile path

doesFileExist :: Member FileSystem r => FilePath -> Eff r Bool
doesFileExist path = send $ DoesFileExist path

createDir :: Member FileSystem r => Directory -> Eff r ()
createDir dir = send $ CreateDir dir

removePath :: Member FileSystem r => FilePath -> Eff r ()
removePath path = send $ RemovePath path

doesDirExist :: Member FileSystem r => Directory -> Eff r Bool
doesDirExist dir = send $ DoesDirExist dir

copyDir :: Member FileSystem r => Directory -> Directory -> Eff r ()
copyDir from to = send $ CopyDir from to

findFiles :: Member FileSystem r
          => (FilePath -> Bool) -> Maybe Directory -> Eff r [FilePath]
findFiles f dir = send $ FindFiles f dir

runFS :: forall e r. LastMember (SafeIO e) r
      => (FSError -> e) -> Eff (FileSystem ': r) ~> Eff r
runFS f = interpretM g where
  g :: FileSystem ~> SafeIO e
  g (WriteFile txt path) = liftSafeIO (f . WriteFileErr path)
                         $ TIO.writeFile path txt
  g (ReadFile path) = liftSafeIO (f . ReadFileErr path) $ TIO.readFile path
  g (DoesFileExist path) = liftSafeIO (f . CheckFileExistsErr path)
                         $ Dir.doesFileExist path
  g (CreateDir dir) = liftSafeIO (f . CreateDirErr dir)
                    $ Dir.createDirectoryIfMissing True dir
  g (RemovePath path) = liftSafeIO (f . RemovePathErr path)
                      $ Dir.removePathForcibly path
  g (DoesDirExist dir) = liftSafeIO (f . CheckDirExistsErr dir)
                       $ Dir.doesDirectoryExist dir
  g (CopyDir from to) =
    let toFilePath = fromText . T.pack
     in liftSafeIO (f . CopyDirErr from to) $
        shelly $ silently $ cp_r (toFilePath from) (toFilePath to)
  g (FindFiles h dir) =
    let toFilePath = fromText . T.pack
        fromFilePath = T.unpack . toTextIgnore
        dir' = fromMaybe "." dir
     in liftSafeIO (f . FindFilesErr dir') $ do
       files <- shelly . silently . find . toFilePath $ dir'
       return . filter h $ fromFilePath <$> files


