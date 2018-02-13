
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Repl ( startRepl ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Idream.Types (Config)


startRepl :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
startRepl = $(logDebug) "startRepl not implemented yet!"
