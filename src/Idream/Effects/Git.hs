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
import Idream.Types.Common (GitCommit, GitUrl)

gitClone :: Directory -> GitUrl -> AppM ()
gitClone repoDir url = procInvokeEnsure_ spec where
  args = ["clone", "--recurse-submodules", "--depth=1", toString url, repoDir]
  spec = Spec "git" args Nothing []

gitFetch :: Directory -> GitCommit -> AppM ()
gitFetch repoDir commit = procInvokeEnsure_ spec where
  args = ["fetch", "--recurse-submodules", "--depth=1", "origin", toString commit]
  spec = Spec "git" args (Just repoDir) []

gitSwitch :: Directory -> GitCommit -> AppM ()
gitSwitch repoDir commit = procInvokeEnsure_ spec where
  args = ["checkout", "--force", toString commit]
  spec = Spec "git" args (Just repoDir) []

gitReadLine :: [Arg] -> Directory -> AppM Text
gitReadLine args repoDir = do
  let spec = Spec "git" args (Just repoDir) []
  Result _ out _ <- procInvokeEnsure spec
  pure (head (T.lines out))

gitReadOriginUrl :: Directory -> AppM GitUrl
gitReadOriginUrl = fmap fromText . gitReadLine ["config", "--get", "remote.origin.url"]

gitReadCurrentBranch :: Directory -> AppM GitCommit
gitReadCurrentBranch repoDir = do
  let args = ["-c", "git symbolic-ref --short -q HEAD || git describe --tags --exact-match 2> /dev/null || git rev-parse --short HEAD"]
      spec = Spec "bash" args (Just repoDir) []
  Result _ out _ <- procInvokeEnsure spec
  pure (fromText (head (T.lines out)))
