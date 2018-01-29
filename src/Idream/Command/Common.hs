
{-# LANGUAGE TemplateHaskell, FlexibleContexts, OverloadedStrings #-}

module Idream.Command.Common ( setupBuildDir
                             , tryAction
                             ) where

-- TODO move to Idream.Helpers?
-- Imports

import Control.Monad.Reader
import Control.Monad.Except
import Control.Exception ( Exception, try )
import System.Directory ( createDirectoryIfMissing )
import Idream.Types ( Config(..), buildDir )


-- Functions

-- | Creates a build directory in which idream will store all build artifacts.
setupBuildDir :: (MonadReader Config m, MonadIO m) => m ()
setupBuildDir = do
  buildDirectory <- asks $ buildDir . buildSettings
  liftIO $ createDirectoryIfMissing True buildDirectory

-- | Helper function for running an IO action in a monad transformer stack,
--   while catching possible exceptions and wrapping them in a custom error type.
tryAction :: (MonadError e' m, MonadIO m, Exception e)
          => (e -> e') -> IO a -> m a
tryAction f = (>>= either (throwError . f) return) <$> liftIO . try
