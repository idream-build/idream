
{-# LANGUAGE RankNTypes #-}

module Idream.Effects.Log ( LogLevel(..), LogError(..), Logger
                          , runLogger, logErr, handleLogErr
                          , debug, info, warn, err
                          ) where

-- Imports

import Prelude hiding ( putStrLn, log )
import Data.Monoid ( (<>) )
import Data.Text ( Text, pack )
import Data.Text.IO ( putStrLn )
import Control.Monad.Freer
import Control.Monad ( when )
import Control.Monad.Reader
import Control.Exception ( IOException )
import Idream.SafeIO
import Idream.Types ( LogLevel(..) )


-- Data types

-- | Data type for storing errors during logging.
data LogError = LogError IOException
  deriving (Eq, Show)

-- | Data type for describing a logging effect.
data Logger a where
  Log :: LogLevel -> Text -> Logger ()


-- Functions

-- Interpreter for the logging effect.
runLogger :: forall e r. LastMember (SafeIO e) r
          => (LogError -> e) -> LogLevel -> Eff (Logger ': r) ~> Eff r
runLogger f thres = interpretM logger where
  logger :: Logger ~> SafeIO e
  logger (Log lvl txt) =
    liftSafeIO (f . LogError) $ runReaderT (runLogger' lvl txt) thres

-- | Helper function for logging when not directly in Eff monad.
runLogger' :: LogLevel -> Text -> ReaderT LogLevel IO ()
runLogger' lvl txt = do
  thres <- ask
  when (lvl >= thres) $ log lvl txt

handleLogErr :: LogError -> Text
handleLogErr (LogError e) =
  pack $ "Error occurred while logging information: " <> show e <> "."

-- Logs a debug message.
debug :: Member Logger r => Text -> Eff r ()
debug txt = send $ Log Debug txt

-- Logs an informational message.
info :: Member Logger r => Text -> Eff r ()
info txt = send $ Log Info txt

-- Logs a warning message.
warn :: Member Logger r => Text -> Eff r ()
warn txt = send $ Log Warn txt

-- Logs an error message.
err :: Member Logger r => Text -> Eff r ()
err txt = send $ Log Err txt

-- Logs a simple message along with a debug level.
log :: MonadIO m => LogLevel -> Text -> m ()
log lvl txt = liftIO $ f lvl where
  f Debug = putStrLn $ "Debug: " <> txt
  f Info  = putStrLn $ "Info: " <> txt
  f Warn  = putStrLn $ "Warn: " <> txt
  f Err   = putStrLn $ "Error: " <> txt

-- | Helper function for logging error messages.
logErr :: MonadIO m => Text -> m ()
logErr = log Err

