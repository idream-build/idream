
module Idream.Command.Test ( runTests ) where

import Control.Monad.Reader
import Idream.Log ( MonadLogger )
import qualified Idream.Log as Log
import Idream.Types (Config)


runTests :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
runTests = Log.debug "runTests not implemented yet!"
