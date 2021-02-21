module Idream.Exe
  ( processCommand
  , main
  ) where

import Idream.App (AppM, newApp, runAppM)
import Idream.Command.Add (addPackageToProject)
-- import Idream.Command.Clean (cleanCode)
-- import Idream.Command.Compile (compileCode)
-- import Idream.Command.Fetch (fetchDeps)
import Idream.Command.GenerateIpkg (generateIpkgFile)
-- import Idream.Command.MkDoc (generateDocs)
import Idream.Command.New (startNewProject)
-- import Idream.Command.Repl (startRepl)
-- import Idream.Command.Run (runCode)
-- import Idream.Command.Test (runTests)
import Idream.OptionParser (parseCmdLineArgs)
import Idream.Types (Args (..), Command (..))

-- | Main function.
main :: IO ()
main = do
  Args severity command <- parseCmdLineArgs
  let app = newApp severity
  runAppM app (processCommand command)

-- | Function that processes the given command.
processCommand :: Command -> AppM ()
processCommand command =
  case command of
    -- Fetch -> fetchDeps
    -- Compile -> compileCode
    -- Clean -> cleanCode
    -- Run runArgs -> runCode runArgs
    -- Repl projName pkgName -> startRepl projName pkgName
    New projName -> startNewProject projName
    Add pkgName pkgType -> addPackageToProject pkgName pkgType
    -- MkDoc -> generateDocs
    GenerateIpkg -> generateIpkgFile
    -- Test -> runTests
    _ -> error "TODO"