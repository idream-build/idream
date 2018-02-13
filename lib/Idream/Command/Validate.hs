
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Validate ( validateConfig ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Idream.Types (Config)


validateConfig :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
validateConfig = $(logDebug) "validateConfig not implemented yet!"
