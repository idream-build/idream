
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.New ( startNewProject ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Idream.Types (Config, PkgName, CodeType)


startNewProject :: (MonadReader Config m, MonadLogger m, MonadIO m)
                => PkgName -> CodeType -> m ()
startNewProject _ _ = $(logDebug) "startNewProject not implemented yet!"
