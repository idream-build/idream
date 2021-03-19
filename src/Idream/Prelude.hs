{-# LANGUAGE FlexibleInstances #-}

module Idream.Prelude
  ( Directory
  , App
  , newApp
  , AppM
  , runAppM
  , ToString (..)
  , IsText (..)
  , ToText (..)
  , module Prelude
  , Exception (..)
  , Generic
  , IOException
  , IsString (..)
  , Map
  , MonadIO (..)
  , Set
  , SomeException
  , Text
  , Void
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
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro (lens)
import LittleLogger (HasSimpleLog (..), Severity, SimpleLogAction, defaultSimpleLogAction, filterActionSeverity,
                     logDebug, logError, logInfo, logWarning)
import LittleRIO (RIO, runRIO)
import Prelude
import System.FilePath ((-<.>), (</>))
import TextShow (showt)
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

-- | Useful for coercing string-like newtypes back to strings
--   without unwrapping everywhere.
--   This is different from 'Show' in that it should form an
--   bijection with 'fromString' from 'IsString' if both are defined.
class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

instance ToString Text where
  toString = T.unpack

-- | Like 'IsString' but again mostly useful for newtype coercions.
class IsText a where
  fromText :: Text -> a

instance IsText String where
  fromText = T.unpack

instance IsText Text where
  fromText = id

-- | Like 'ToString'.
class ToText a where
  toText :: a -> Text

instance ToText String where
  toText = T.pack

instance ToText Text where
  toText = id

instance ToText Int where
  toText = showt
