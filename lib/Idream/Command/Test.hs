
module Idream.Command.Test ( runTests ) where

import Control.Monad.Reader
import Idream.Effects.Log (logErr)

runTests :: MonadIO m => m ()
runTests = logErr "runTests not implemented yet!"
