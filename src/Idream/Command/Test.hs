module Idream.Command.Test
  ( testImpl
  ) where

import Idream.App (AppM)
import Idream.Command.Common (readDepInfoMap, readResolvedProject)
import Idream.FilePaths (Directory)
import Idream.Types.Common (PackageGroup, RefreshStrategy)

testImpl :: Directory -> PackageGroup -> RefreshStrategy -> AppM ()
testImpl projDir group refreshStrat = do
  rp <- readResolvedProject projDir
  dim <- readDepInfoMap projDir rp
  error "TODO - finish test"
