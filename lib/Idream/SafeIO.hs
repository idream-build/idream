
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Idream.SafeIO ( SafeIO
                     , MonadIO(..)
                     , MonadSafeIO(..)
                     , runSafeIO
                     , mapError
                     , liftEither
                     ) where


-- Imports

import Control.Exception (Exception, try)
import Control.Monad.Except hiding (liftEither)


-- Data types

-- | Type used for describing IO actions that can fail.
newtype SafeIO e a = SafeIO (ExceptT e IO a)
  deriving (Functor, Applicative, Monad)


-- Type classes

-- | Type class used for ensuring that exceptions that occur in IO monad are handled.
--   It is mostly based on MonadError typeclass, with some small modifications.
class Monad m => MonadSafeIO e m | m -> e where
  -- | Lifts an IO action into MonadSafeIO.
  --   An error handling function needs to be provided in order to handle possible exceptions.
  liftSafeIO :: Exception exc => (exc -> e) -> IO a -> m a
  -- | Used to notify the computation of an error so it can be handled later on.
  raiseError :: e -> m a


-- Instances

instance MonadSafeIO e (SafeIO e) where
  liftSafeIO f a = SafeIO $ do
    result <- liftIO $ try a
    either (throwError . f) return result
  raiseError e = SafeIO $ throwError e
  {-# INLINE liftSafeIO #-}
  {-# INLINE raiseError #-}


-- Functions

-- | Runs a safe IO computation that can possibly fail.
runSafeIO :: SafeIO e a -> IO (Either e a)
runSafeIO (SafeIO m) = runExceptT m

-- | Helper function for converting error type of a SafeIO action.
mapError :: (e1 -> e2) -> SafeIO e1 a -> SafeIO e2 a
mapError f (SafeIO m) = SafeIO $ withExceptT f m

-- | Lifts an either computation into a MonadSafeIO.
liftEither :: MonadSafeIO e m => Either e a -> m a
liftEither = either raiseError return

