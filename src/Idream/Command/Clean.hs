module Idream.Command.Clean
  ( cleanImpl
  ) where

import Control.Monad (when)
import Idream.App (AppM)
import Idream.Effects.FileSystem (fsDoesDirectoryExist, fsRemovePath)
import Idream.FileLogic (workDir)
import Idream.FilePaths (Directory)
import LittleLogger (logInfo)
import System.FilePath ((</>))

cleanImpl :: Directory -> AppM ()
cleanImpl projDir = do
  let projWorkDir = projDir </> workDir
  exists <- fsDoesDirectoryExist projWorkDir
  when exists $ do
    logInfo "Cleaning work directory"
    fsRemovePath projWorkDir
