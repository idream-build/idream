
module Idream.Command.Common ( tryAction ) where

-- Imports

import Control.Monad.Except
import Control.Exception ( Exception, try )


-- Functions

-- | Helper function for running an IO action in a monad transformer stack,
--   while catching possible exceptions and wrapping them in a custom error type.
tryAction :: (MonadError e' m, MonadIO m, Exception e)
          => (e -> e') -> IO a -> m a
tryAction f = (>>= either (throwError . f) return) <$> liftIO . try
