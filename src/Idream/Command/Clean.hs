
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Clean (cleanCode) where

import Control.Monad.Reader
import Control.Monad.Logger
import Idream.Types (Config)


cleanCode :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
cleanCode = $(logDebug) "cleanCode not implemented yet!"
