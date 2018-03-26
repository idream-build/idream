
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Idream.Log ( Level(..), LoggingT(..), MonadLogger(..)
                  , debug, info, warn, err
                  ) where

-- Imports

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.Text ( Text )
import Data.Monoid ( (<>) )


-- Data types

-- | Data type enumerating the various log levels.
data Level = Debug | Info | Warn | Err
           deriving (Eq, Ord, Show)

-- | Custom logging monad transformer, can be configured to only output
--   logging messages at same level or above logging threshold.
newtype LoggingT a =
  LoggingT { runLoggingT :: ReaderT Level IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Level)

-- | Type class used for logging.
class Monad m => MonadLogger m where
  logGeneric :: Level -> Text -> m ()


-- Instances

instance MonadLogger LoggingT where
  logGeneric = logGeneric'

instance MonadLogger m => MonadLogger (ReaderT r m) where
  logGeneric lvl = lift . logGeneric lvl

instance MonadLogger m => MonadLogger (ExceptT e m) where
  logGeneric lvl = lift . logGeneric lvl

instance MonadLogger m => MonadLogger (StateT s m) where
  logGeneric lvl = lift . logGeneric lvl


-- Functions

-- | Helper functions for logging a message.
debug, info, warn, err :: MonadLogger m => Text -> m ()
debug = logGeneric Debug
info = logGeneric Info
warn = logGeneric Warn
err = logGeneric Err

-- | Helper function that does the actual logging.
logGeneric' :: Level -> Text -> LoggingT ()
logGeneric' lvl txt = do
  logThreshold <- ask
  when (lvl >= logThreshold) (liftIO . print $ lvlToTxt lvl <> txt)

-- | Conversion function from logging level to text.
lvlToTxt :: Level -> Text
lvlToTxt Debug = "Debug: "
lvlToTxt Info = "Info: "
lvlToTxt Warn = "Warning: "
lvlToTxt Err = "Error: "

