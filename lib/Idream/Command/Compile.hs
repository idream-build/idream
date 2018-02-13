
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Compile ( compileCode ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Idream.Types (Config)

compileCode :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
compileCode = $(logDebug) "compileCode not implemented yet!"
