
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Idream.SafeIO ( SafeIO
                     , MonadSafeIO(..)
                     , runSafeIO
                     , mapError
                     , liftEither
                     ) where


-- Imports

import Control.Exception ( try, Exception )
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader


-- Data types


-- | Type used for describing IO actions that can fail.
newtype SafeIO e m a = SafeIO (ExceptT e m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadReader r)


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


instance MonadIO m => MonadSafeIO e (SafeIO e m) where
  liftSafeIO f a = SafeIO $ do
    result <- liftIO $ try a
    either (throwError . f) return result
  raiseError e = SafeIO $ throwError e
  {-# INLINE liftSafeIO #-}
  {-# INLINE raiseError #-}


-- The following instances need UndecidableInstances in order to compile.

instance MonadSafeIO e m => MonadSafeIO e (StateT s m) where
  liftSafeIO f a = lift $ liftSafeIO f a
  raiseError e = lift $ raiseError e


-- Functions

-- | Runs a safe IO computation that can possibly fail.
runSafeIO :: SafeIO e m a -> m (Either e a)
runSafeIO (SafeIO m) = runExceptT m

-- | Helper function for converting error type of a SafeIO action.
mapError :: Functor m => (e1 -> e2) -> SafeIO e1 m a -> SafeIO e2 m a
mapError f (SafeIO m) = SafeIO $ withExceptT f m

-- | Lifts an either computation into a MonadSafeIO.
liftEither :: MonadSafeIO e m => Either e a -> m a
liftEither = either raiseError return

