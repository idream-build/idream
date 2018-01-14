
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.New ( startNewProject ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Idream.Types (Config, PackageName, PackageType)


startNewProject :: (MonadReader Config m, MonadLogger m, MonadIO m)
                => PackageName -> PackageType -> m ()
startNewProject _ _ = $(logDebug) "startNewProject not implemented yet!"
