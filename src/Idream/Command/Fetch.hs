
{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Idream.Command.Fetch ( fetchDeps ) where

import Control.Monad.Reader
import Control.Monad.Logger
import Idream.Types (Config)

fetchDeps :: (MonadReader Config m, MonadLogger m, MonadIO m) => m ()
fetchDeps = do
  $(logDebug) "Fetching dependencies."



  return ()


{- TODO:

fetch all deps listed in json file of project => store in .idream-work/src/LIB_NAME
start building graph along the way, to check for cycles/or already fetched deps
repeat until its fully fetched
cache graph somewhere in a file (as json)?

problems: version differences?

-}
