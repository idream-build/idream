
module Idream.Command.Test ( runTests ) where

import Control.Monad.Reader
import Idream.Log ( logErr )

runTests :: MonadIO m => m ()
runTests = logErr "runTests not implemented yet!"
