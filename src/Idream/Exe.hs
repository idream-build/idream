module Idream.Exe
  ( processCommand
  , main
  ) where

import Control.Monad.Reader (asks)
import Idream.Command.Add (addPackageToProject)
import Idream.Command.Clean (cleanCode)
import Idream.Command.Compile (compileCode)
import Idream.Command.Fetch (fetchDeps)
import Idream.Command.GenerateIpkg (generateIpkgFile)
import Idream.Command.MkDoc (generateDocs)
import Idream.Command.New (startNewProject)
import Idream.Command.Repl (startRepl)
import Idream.Command.Run (runCode)
import Idream.Command.Test (runTests)
import Idream.OptionParser (parseCmdLineArgs)
import Idream.Types (Args (..), Command (..), Config (..))
import LittleRIO (RIO, runRIO)

-- | Main function.
main :: IO ()
main = do
  cmdLineArgs <- parseCmdLineArgs
  let config = Config cmdLineArgs
  runRIO config $ do
    command <- asks $ cmd . args
    processCommand command

-- | Function that processes the given command.
processCommand :: Command -> RIO Config ()
processCommand command =
  case command of
    Fetch -> fetchDeps
    Compile -> compileCode
    Clean -> cleanCode
    Run runArgs -> runCode runArgs
    Repl projName pkgName -> startRepl projName pkgName
    New projName -> startNewProject projName
    Add pkgName pkgType -> addPackageToProject pkgName pkgType
    MkDoc -> generateDocs
    GenerateIpkg -> generateIpkgFile
    Test -> runTests
