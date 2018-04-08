
module Idream.Command.Repl ( startRepl ) where

import Control.Monad.Reader
import Idream.Log ( MonadLogger )
import qualified Idream.Log as Log
import Idream.Types (Config)


startRepl :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
startRepl = Log.debug "startRepl not implemented yet!"
