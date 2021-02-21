module Idream.Effects.Git
  ( gitCheckout
  , gitClone
  ) where

import Idream.App (AppM)
import Idream.FilePaths (Directory)
import Idream.Types (Repo (..), Version (..))

gitCheckout :: Repo -> Version -> Directory -> AppM ()
gitCheckout _repo _version _dir = error "TODO: git checkout"

gitClone :: Repo -> Directory -> AppM ()
gitClone _repo _dir = error "TODO: git clone"
