
module Main where

-- Imports

import Control.Monad.Reader
import Idream.OptionParser
import Idream.Types
import Idream.Command.Fetch (fetchDeps)
import Idream.Command.Compile (compileCode)
import Idream.Command.Clean (cleanCode)
import Idream.Command.Run (runCode)
import Idream.Command.Repl (startRepl)
import Idream.Command.New (startNewProject)
import Idream.Command.Add (addPackageToProject)
import Idream.Command.MkDoc (generateDocs)
import Idream.Command.GenerateIpkg (generateIpkgFile)
import Idream.Command.Test (runTests)
import System.Directory (setCurrentDirectory)


-- Functions

-- | Main function.
main :: IO ()
main = do
  cmdLineArgs <- parseCmdLineArgs
  let config = Config cmdLineArgs
  flip runReaderT config $ do
    argsVal <- asks args
    liftIO $ setCurrentDirectory (projRoot argsVal)
    processCommand (cmd argsVal)

-- | Function that processes the given command.
processCommand :: ( MonadReader Config m, MonadIO m )
               => Command -> m ()
processCommand Fetch = fetchDeps
processCommand Compile = compileCode
processCommand Clean = cleanCode
processCommand (Run runArgs) = runCode runArgs
processCommand (Repl pkgName) = startRepl pkgName
processCommand (New projName) = startNewProject projName
processCommand (Add pkgName pkgType) = addPackageToProject pkgName pkgType
processCommand MkDoc = generateDocs
processCommand GenerateIpkg = generateIpkgFile
processCommand Test = runTests
