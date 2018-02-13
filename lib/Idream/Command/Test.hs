
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Test ( runTests ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Idream.Types (Config)


runTests :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
runTests = $(logDebug) "runTests not implemented yet!"
