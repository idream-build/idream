module Idream.Exe
  ( processCommand
  , main
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Idream.App (AppM, newApp, runAppM)
import Idream.Command.Add (addImpl)
import Idream.Command.Clean (cleanImpl)
import Idream.Command.Common (PackageGroup (..))
import Idream.Command.Compile (compileImpl)
import Idream.Command.Fetch (fetchImpl)
import Idream.Command.New (newImpl)
import Idream.Command.Test (testImpl)
import Idream.FilePaths (Directory)
import Idream.OptionParser (parseCmdLineArgs)
import Idream.Types.Command (Args (..), Command (..))
import Idream.Types.Common (PackageName (..), ProjectName (..), RefreshStrategy)

-- | Main function.
main :: IO ()
main = do
  Args severity mayProjDir refreshStrat command <- parseCmdLineArgs
  let projDir = effectiveProjDir mayProjDir command
      app = newApp severity
  runAppM app (processCommand projDir refreshStrat command)

-- | Gets the project dir as configured through args or convention.
effectiveProjDir :: Maybe Directory -> Command -> Directory
effectiveProjDir mayProjDir cmd =
  case mayProjDir of
    Nothing ->
      case cmd of
        New jn -> T.unpack (unProjName jn)
        _ -> "."
    Just d -> d

-- | Function that processes the given command.
processCommand :: Directory -> RefreshStrategy -> Command -> AppM ()
processCommand projDir refreshStrat command =
  case command of
    Fetch pkgGroup ->
      fetchImpl projDir pkgGroup refreshStrat
    Compile pkgGroup ->
      compileImpl projDir pkgGroup refreshStrat
    Clean -> cleanImpl projDir
    New projName -> newImpl projDir projName
    Add mayPkgSubDir pkgName pkgType ->
      addImpl projDir mayPkgSubDir pkgName pkgType
    Test pkgGroup -> testImpl projDir pkgGroup refreshStrat
    _ -> error ("TODO implement command: " <> show command)
