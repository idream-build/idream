module Idream.Effects.Git
  ( gitEnsure
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Idream.App (AppM)
import Idream.FilePaths (Directory)
import Idream.Effects.Process (Result (..), Spec (..), procInvokeEnsure, procInvokeEnsure_)
import Idream.Effects.FileSystem (fsDoesDirectoryExist, fsRemovePath)
import LittleLogger (logInfo)

gitEnsure :: Directory -> Text -> Text -> AppM ()
gitEnsure repoDir url commit = do
  repoExists <- fsDoesDirectoryExist repoDir
  if repoExists
    then do
      existingUrl <- gitOriginUrl repoDir
      if existingUrl == url
        then do
          logInfo ("Fetching " <> commit)
          gitFetch repoDir commit
          logInfo ("Switching " <> commit)
          gitSwitch repoDir commit
        else do
          logInfo ("Re-cloning " <> url <> " at " <> commit)
          fsRemovePath repoDir
          gitClone repoDir url commit
    else do
      logInfo ("Cloning " <> url <> " at " <> commit)
      gitClone repoDir url commit

gitClone :: Directory -> Text -> Text -> AppM ()
gitClone repoDir url commit = procInvokeEnsure_ spec where
  args = ["clone", "--recurse-submodules", "--depth=1", T.unpack ("--branch=" <> commit), repoDir]
  spec = Spec "git" args Nothing []

gitFetch :: Directory -> Text -> AppM ()
gitFetch repoDir commit = procInvokeEnsure_ spec where
  args = ["fetch", "--recurse-submodules", "--depth=1", T.unpack commit]
  spec = Spec "git" args (Just repoDir) []

gitSwitch :: Directory -> Text -> AppM ()
gitSwitch repoDir commit = procInvokeEnsure_ spec where
  args = ["switch", "--force", T.unpack commit]
  spec = Spec "git" args (Just repoDir) []

gitOriginUrl :: Directory -> AppM Text
gitOriginUrl repoDir = do
  let args = ["config", "--get", "remote.origin.url"]
      spec = Spec "git" args (Just repoDir) []
  Result _ out _ <- procInvokeEnsure spec
  pure (head (T.lines out))
