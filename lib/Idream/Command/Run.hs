
module Idream.Command.Run ( runCode ) where

import Control.Monad.Reader
import Idream.Log ( logErr )
import Idream.Types (Argument)


runCode :: MonadIO m => [Argument] -> m ()
runCode _ = logErr "runCode not implemented yet!"
