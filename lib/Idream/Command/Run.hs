
module Idream.Command.Run ( runCode ) where

import Control.Monad.Reader
import Idream.Log ( MonadLogger )
import qualified Idream.Log as Log
import Idream.Types (Config, Argument)

runCode :: (MonadReader Config m, MonadLogger m, MonadIO m)
        => [Argument] -> m ()
runCode _ = Log.debug "runCode not implemented yet!"
