
module Idream.Command.MkDoc ( generateDocs ) where

import Control.Monad.Reader
import Idream.Log ( MonadLogger )
import qualified Idream.Log as Log
import Idream.Types (Config)


generateDocs :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
generateDocs = Log.debug "generateDocs not implemented yet!"
