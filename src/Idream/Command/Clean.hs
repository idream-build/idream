module Idream.Command.Clean
  ( clean
  ) where

import Control.Monad (when)
import Idream.App (AppM)
import Idream.Effects.FileSystem (fsDoesDirectoryExist, fsRemovePath)
import Idream.FileLogic (workDir)
import Idream.FilePaths (Directory)
import LittleLogger (logInfo)
import System.FilePath ((</>))

clean :: Directory -> AppM ()
clean projDir = do
  let projWorkDir = projDir </> workDir
  exists <- fsDoesDirectoryExist projWorkDir
  when exists $ do
    logInfo "Cleaning work directory"
    fsRemovePath projWorkDir
