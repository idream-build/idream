
module Idream.Command.Repl ( startRepl ) where

import Control.Monad.Reader
import Idream.Effects.Log ( logErr )


startRepl :: MonadIO m => m ()
startRepl = logErr "startRepl not implemented yet!"
