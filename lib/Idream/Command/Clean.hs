
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Clean (cleanCode) where

-- Imports

import Control.Monad.Reader
import Idream.Log ( MonadLogger )
import qualified Idream.Log as Log
import Idream.Types (Config(..), BuildSettings(..))
import System.Directory ( getCurrentDirectory
                        , listDirectory
                        , removePathForcibly )
import System.FilePath ( (</>) )
import Control.Exception ( try, IOException )
import qualified Data.Text as T
import Data.Monoid ( (<>) )


-- Functions

-- | Cleans up the working directory of a project.
cleanCode :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
cleanCode = do
  Log.info "Cleaning project."
  workDir <- asks $ buildDir . buildSettings
  result <- liftIO . try $ do
    cwd <- getCurrentDirectory
    files <- listDirectory $ cwd </> workDir
    mapM_ removePathForcibly files
  either showError return result

-- | Displays the error if something went wrong during project cleanup.
showError :: MonadLogger m => IOException -> m ()
showError e = Log.err ("Failed to clean project: " <> T.pack (show e))
