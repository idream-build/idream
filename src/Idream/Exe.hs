module Idream.Exe
  ( processCommand
  , main
  ) where

import Idream.Command.Add (addImpl)
import Idream.Command.Clean (cleanImpl)
import Idream.Command.Compile (compileImpl)
import Idream.Command.Fetch (fetchImpl)
import Idream.Command.New (newImpl)
import Idream.Command.Repl (replImpl)
import Idream.Command.Test (testImpl)
import Idream.OptionParser (parseCmdLineArgs)
import Idream.Prelude
import Idream.Types.Command (Args (..), Command (..))

-- | Main function.
main :: IO ()
main = do
  Args severity mayProjDir command <- parseCmdLineArgs
  let projDir = effectiveProjDir mayProjDir command
      app = newApp severity
  runAppM app (processCommand projDir command)

-- | Gets the project dir as configured through args or convention.
effectiveProjDir :: Maybe Directory -> Command -> Directory
effectiveProjDir mayProjDir cmd =
  case mayProjDir of
    Nothing ->
      case cmd of
        New jn -> toString jn
        _ -> "."
    Just d -> d

-- | Function that processes the given command.
processCommand :: Directory -> Command -> AppM ()
processCommand projDir command =
  case command of
    Fetch pkgGroup refreshStrat ->
      fetchImpl projDir pkgGroup refreshStrat
    Compile pkgGroup ->
      compileImpl projDir pkgGroup
    Clean -> cleanImpl projDir
    New projName -> newImpl projDir projName
    Add mayPkgSubDir pkgName pkgType ->
      addImpl projDir mayPkgSubDir pkgName pkgType
    Test pkgGroup -> testImpl projDir pkgGroup
    Repl pkgName -> replImpl projDir pkgName
    _ -> error ("TODO implement command: " <> show command)
