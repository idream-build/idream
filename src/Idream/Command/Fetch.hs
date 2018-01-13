
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Fetch ( fetchDeps ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Idream.Types (Config)

fetchDeps :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
fetchDeps = $(logDebug) "fetchDeps not implemented yet!"
