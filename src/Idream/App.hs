module Idream.App
  ( App (..)
  , newApp
  , AppM
  , runAppM
  ) where

import Control.Monad.IO.Class (MonadIO)
import Lens.Micro (lens)
import LittleLogger (HasSimpleLog (..), Severity, SimpleLogAction, defaultSimpleLogAction, filterActionSeverity)
import LittleRIO (RIO, runRIO)

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
