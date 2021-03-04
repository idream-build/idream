module Idream.Effects.Git
  ( gitClone
  , gitFetch
  , gitSwitch
  , gitReadOriginUrl
  , gitReadCurrentBranch
  ) where

import qualified Data.Text as T
import Idream.Effects.Process (Arg, Result (..), Spec (..), procInvokeEnsure, procInvokeEnsure_)
import Idream.Prelude

gitClone :: Directory -> Text -> Text -> AppM ()
gitClone repoDir url commit = procInvokeEnsure_ spec where
  args = ["clone", "--recurse-submodules", "--depth=1", T.unpack ("--branch=" <> commit), T.unpack url, repoDir]
  spec = Spec "git" args Nothing []

gitFetch :: Directory -> Text -> AppM ()
gitFetch repoDir commit = procInvokeEnsure_ spec where
  args = ["fetch", "--recurse-submodules", "--depth=1", "origin", T.unpack commit]
  spec = Spec "git" args (Just repoDir) []

gitSwitch :: Directory -> Text -> AppM ()
gitSwitch repoDir commit = procInvokeEnsure_ spec where
  args = ["switch", "--force", T.unpack commit]
  spec = Spec "git" args (Just repoDir) []

gitReadLine :: [Arg] -> Directory -> AppM Text
gitReadLine args repoDir = do
  let spec = Spec "git" args (Just repoDir) []
  Result _ out _ <- procInvokeEnsure spec
  pure (head (T.lines out))

gitReadOriginUrl :: Directory -> AppM Text
gitReadOriginUrl = gitReadLine ["config", "--get", "remote.origin.url"]

gitReadCurrentBranch :: Directory -> AppM Text
gitReadCurrentBranch repoDir = do
  let args = ["-c", "git symbolic-ref --short -q HEAD || git describe --tags --exact-match 2> /dev/null || git rev-parse --short HEAD"]
      spec = Spec "bash" args (Just repoDir) []
  Result _ out _ <- procInvokeEnsure spec
  pure (head (T.lines out))
