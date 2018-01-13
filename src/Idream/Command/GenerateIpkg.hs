
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.GenerateIpkg ( generateIpkgFile ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Idream.Types (Config)


generateIpkgFile :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
generateIpkgFile = $(logDebug) "generateIpkgFile not implemented yet!"
