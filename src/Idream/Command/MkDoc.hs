
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.MkDoc ( generateDocs ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Idream.Types (Config)


generateDocs :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
generateDocs = $(logDebug) "generateDocs not implemented yet!"
