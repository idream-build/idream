
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Run ( runCode ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Idream.Types (Config, Argument)

runCode :: (MonadReader Config m, MonadLogger m, MonadIO m)
        => [Argument] -> m ()
runCode _ = $(logDebug) "runCode not implemented yet!"
