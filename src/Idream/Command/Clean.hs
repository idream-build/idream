module Idream.Command.Clean
  ( cleanImpl
  ) where

import Idream.Effects.FileSystem (fsDoesDirectoryExist, fsRemovePath)
import Idream.FileLogic (workDir)
import Idream.Prelude

cleanImpl :: Directory -> AppM ()
cleanImpl projDir = do
  let projWorkDir = projDir </> workDir
  exists <- fsDoesDirectoryExist projWorkDir
  when exists $ do
    logInfo "Cleaning work directory"
    fsRemovePath projWorkDir
