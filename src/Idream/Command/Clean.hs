
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Clean (cleanCode) where

-- Imports

import Control.Monad.Reader
import Control.Monad.Logger
import Idream.Types (Config)
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
  $(logInfo) "Cleaning project."
  result <- liftIO . try $ do
    cwd <- getCurrentDirectory
    files <- listDirectory $ cwd </> ".idream-work"
    mapM_ removePathForcibly files
  either showError return result

-- | Displays the error if something went wrong during project cleanup.
showError :: MonadLogger m => IOException -> m ()
showError e = $(logError) ("Failed to clean project: " <> T.pack (show e))
