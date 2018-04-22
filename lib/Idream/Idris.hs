
{-# LANGUAGE RankNTypes #-}

module Idream.Idris ( Idris(..), IdrisError(..)
                    , Command, Arg, Environment
                    , idrisCompile
                    , runIdris
                    , handleIdrisErr
                    ) where

-- Imports

import Control.Monad.Freer
import Control.Exception ( IOException )
import System.Process ( createProcess, waitForProcess, env, proc )
import System.Exit ( ExitCode(..) )
import Idream.SafeIO
import Idream.Types ( ProjectName(..), PackageName(..) )
import Data.Monoid ( (<>) )
import qualified Data.Text as T


-- Data types

data Idris a where
  IdrisCompile :: ProjectName -> PackageName -> [Arg] -> Environment -> Idris ()

data IdrisError = IdrCompileErr ProjectName PackageName IOException
                | IdrCommandErr ProjectName PackageName Command ExitCode
  deriving (Eq, Show)


-- | Type alias for command when spawning an external OS process.
type Command = String

-- | Type alias for command line arguments when spawning an external OS process.
type Arg = String

-- | Type alias for an environment to be passed to a command,
--   expressed as a list of key value pairs.
type Environment = [(String, String)]


-- Functions


idrisCompile :: Member Idris r
             => ProjectName -> PackageName -> [Arg] -> Environment -> Eff r ()
idrisCompile projName pkgName args environ =
  send $ IdrisCompile projName pkgName args environ

runIdris :: forall e r. LastMember (SafeIO e) r
         => (IdrisError -> e) -> Eff (Idris ': r) ~> Eff r
runIdris f = interpretM g where
  g :: Idris ~> SafeIO e
  g (IdrisCompile projName pkgName args environ) =
    let eh1 = IdrCompileErr projName pkgName
        eh2 = IdrCommandErr projName pkgName "compile"
     in invokeIdrisWithEnv (f . eh1) (f . eh2) args environ


-- | Invokes a command as a separate operating system process.
--   Allows passing additional environment variables to the external process.
invokeCmdWithEnv :: (IOException -> e)
                 -> (ExitCode -> e)
                 -> Command -> [Arg] -> Environment
                 -> SafeIO e ()
invokeCmdWithEnv f g cmd cmdArgs environ = do
  result <- liftSafeIO f $ do
    let process = (proc cmd cmdArgs) { env = Just environ }
    (_, _, _, procHandle) <- createProcess process
    waitForProcess procHandle
  if result == ExitSuccess then return () else raiseError (g result)

invokeIdrisWithEnv :: (IOException -> e)
                   -> (ExitCode -> e)
                   -> [Arg] -> Environment
                   -> SafeIO e ()
invokeIdrisWithEnv f g = invokeCmdWithEnv f g "idris"

handleIdrisErr :: IdrisError -> T.Text
handleIdrisErr (IdrCompileErr projName pkgName err) =
  "Failed to compile idris package (project = "
    <> unProjName projName <> ", package = " <> unPkgName pkgName
    <> "), reason: " <> T.pack (show err) <> "."
handleIdrisErr (IdrCommandErr projName pkgName cmd exitCode) =
  "Failed to invoke idris (command: " <> T.pack cmd <> ") for project "
    <> unProjName projName <> ", package = " <> unPkgName pkgName
    <> ", exit code = " <> T.pack (show exitCode) <> "."

