
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Clean ( cleanCode ) where

-- Imports

import Control.Monad.Reader
import Control.Exception ( IOException )
import Idream.Log ( MonadLogger )
import qualified Idream.Log as Log
import Idream.SafeIO
import Idream.Types ( Config(..), BuildSettings(..), Directory )
import System.Directory ( removePathForcibly )
import qualified Data.Text as T
import Data.Monoid ( (<>) )


-- Data Types

newtype CleanErr = CleanErr IOException deriving (Eq, Show)


-- Functions

-- | Cleans up the working directory of a project.
cleanCode :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
cleanCode = do
  Log.info "Cleaning project."
  workDir <- asks $ buildDir . buildSettings
  result <- runSafeIO $ cleanCode' workDir
  either showError return result

cleanCode' :: ( MonadLogger m, MonadSafeIO CleanErr m ) => Directory -> m ()
cleanCode' workDir = liftSafeIO CleanErr $ removePathForcibly workDir

-- | Displays the error if something went wrong during project cleanup.
showError :: MonadLogger m => CleanErr -> m ()
showError (CleanErr e) =
  Log.err ("Failed to clean project: " <> T.pack (show e))

