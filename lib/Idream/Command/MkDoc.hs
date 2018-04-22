
module Idream.Command.MkDoc ( generateDocs ) where

import Control.Monad.Reader
import Idream.Log ( logErr )


generateDocs :: MonadIO m => m ()
generateDocs = logErr "generateDocs not implemented yet!"
