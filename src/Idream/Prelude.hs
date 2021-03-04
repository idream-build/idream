module Idream.Prelude
  ( Directory
  , App
  , newApp
  , AppM
  , runAppM
  , module Prelude
  , Exception (..)
  , IOException
  , Map
  , MonadIO (..)
  , Set
  , SomeException
  , Text
  , (</>)
  , (-<.>)
  , catch
  , catchIO
  , for
  , for_
  , fromMaybe
  , intercalate
  , logDebug
  , logError
  , logInfo
  , logWarning
  , join
  , throwIO
  , toList
  , unless
  , void
  , when
  ) where

import Control.Exception (Exception (..), IOException, SomeException)
import Control.Monad (join, unless, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_, toList)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable (for)
import Lens.Micro (lens)
import LittleLogger (HasSimpleLog (..), Severity, SimpleLogAction, defaultSimpleLogAction, filterActionSeverity,
                     logDebug, logError, logInfo, logWarning)
import LittleRIO (RIO, runRIO)
import Prelude
import System.FilePath ((</>), (-<.>))
import UnliftIO.Exception (catch, catchIO, throwIO)

type Directory = FilePath

newtype App = App
  { appSimpleLog :: SimpleLogAction
  }

instance HasSimpleLog App where
  simpleLogL = lens appSimpleLog (\c l -> c { appSimpleLog = l })

newApp :: Severity -> App
newApp severity = App (filterActionSeverity severity defaultSimpleLogAction)

type AppM a = RIO App a

runAppM :: MonadIO m => App -> AppM a -> m a
runAppM = runRIO
