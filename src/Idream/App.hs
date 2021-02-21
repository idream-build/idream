module Idream.App
  ( App (..)
  , newApp
  , AppM
  , runAppM
  , appLogAndThrow
  , appCopyDir
  , appCreateDir
  , appDoesDirectoryExist
  , appFindFiles
  , appReadJSON
  , appRemovePath
  , appWriteJSON
  , appWriteFile
  ) where

import Control.Exception (Exception, IOException)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict')
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Idream.FilePaths (Directory)
import Idream.ToText (ToText (..))
import Lens.Micro (lens)
import LittleLogger (HasSimpleLog (..), Severity, SimpleLogAction, defaultSimpleLogAction, filterActionSeverity, logError)
import LittleRIO (RIO, runRIO)
import UnliftIO.Directory (createDirectoryIfMissing, doesDirectoryExist)
import UnliftIO.Exception (catchIO)

data App = App
  { appSimpleLog :: SimpleLogAction
  }

instance HasSimpleLog App where
  simpleLogL = lens appSimpleLog (\c l -> c { appSimpleLog = l })

newApp :: Severity -> App
newApp severity = App (filterActionSeverity severity defaultSimpleLogAction)

type AppM a = RIO App a

runAppM :: MonadIO m => App -> AppM a -> m a
runAppM = runRIO

appLogAndThrow :: (ToText e, Exception e) => e -> AppM x
appLogAndThrow e = do
  logError (toText e)
  throwM e

data FSError =
    WriteFileErr FilePath IOException
  | ReadFileErr FilePath IOException
  | CreateDirErr Directory IOException
  deriving (Eq, Show)

instance Exception FSError

instance ToText FSError where
  toText e =
    case e of
      WriteFileErr path err ->
        "Failed to write to file (" <> toText path <> "), reason: " <> toText err <> "."
      ReadFileErr path err ->
        "Failed to read from file (" <> toText path <> "), reason: " <> toText err <> "."
      CreateDirErr dir err ->
        "Failed to create directory (" <> toText dir <> "), reason: " <> toText err <> "."

appCopyDir :: Directory -> Directory -> AppM ()
appCopyDir _fromDir _toDir = error "TODO: copy dir"

appCreateDir :: Directory -> AppM ()
appCreateDir dir = catchIO (createDirectoryIfMissing False dir) (appLogAndThrow . CreateDirErr dir)

appDoesDirectoryExist :: Directory -> AppM Bool
appDoesDirectoryExist = doesDirectoryExist

appFindFiles :: (FilePath -> Bool) -> Maybe Directory -> AppM [FilePath]
appFindFiles _pcate _mayDir = error "TODO: find files"

appReadJSON :: (ToText e, Exception e, FromJSON a) => (String -> e) -> FilePath -> AppM a
appReadJSON conv path = do
  ep <- catchIO (liftIO (eitherDecodeFileStrict' path)) (appLogAndThrow . ReadFileErr path)
  case ep of
    Left err -> appLogAndThrow (conv err)
    Right val -> pure val

appRemovePath :: FilePath -> AppM ()
appRemovePath _path = error "TODO: remove path"

prettyConfig :: Config
prettyConfig = defConfig { confIndent = Spaces 2, confTrailingNewline = True }

appWriteJSON :: ToJSON a => FilePath -> a -> AppM ()
appWriteJSON path val = do
  let pretty = encodePretty' prettyConfig val
  catchIO (liftIO (BSL.writeFile path pretty)) (appLogAndThrow . WriteFileErr path)

appWriteFile :: FilePath -> Text -> AppM ()
appWriteFile path txt = catchIO (liftIO (TIO.writeFile path txt)) (appLogAndThrow . WriteFileErr path)
