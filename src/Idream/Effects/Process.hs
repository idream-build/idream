module Idream.Effects.Process
  ( Command
  , Arg
  , Environment
  , Spec (..)
  , Result (..)
  , procInvoke
  , procEnsureSuccess
  , procInvokeEnsure
  , procInvokeEnsure_
  , procDebug_
  ) where

import Control.Exception (Exception (..))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Idream.App (AppM)
import Idream.FilePaths (Directory)
import System.Exit (ExitCode (..))
import UnliftIO.Environment (getEnv)
import UnliftIO.Exception (throwIO)
import UnliftIO.Process (StdStream (..), createProcess, cwd, env, proc, std_err, std_out, waitForProcess)

-- | Type alias for command when spawning an external OS process.
type Command = String

-- | Type alias for command line arguments when spawning an external OS process.
type Arg = String

-- | Type alias for an environment to be passed to a command,
--   expressed as a list of key value pairs.
type Environment = [(String, String)]

data Spec = Spec
  { specCmd :: Command
  , specCmdArgs :: [Arg]
  , specWorkDir :: Maybe Directory
  , specEnviron :: Environment
  } deriving (Show, Eq)

data Result = Result
  { resultCode :: ExitCode
  , resultOut :: Text
  , resultErr :: Text
  } deriving (Show, Eq)

data ProcessFailedErr = ProcessFailedErr Spec Int Text Text
  deriving (Show, Eq)

instance Exception ProcessFailedErr where
  displayException (ProcessFailedErr (Spec cmd cmdArgs maybeWorkDir _) failCode out err) =
    concat
      [ "Command exited with code "
      , show failCode
      , ": [ "
      , unwords (cmd:cmdArgs)
      , " ]"
      , maybe "" (" in " <>) maybeWorkDir
      , "\nStdout:\n"
      , T.unpack out
      , "\nStderr:\n"
      , T.unpack err
      ]

-- | Invokes a command as a separate operating system process.
--   Allows passing additional environment variables to the external process.
procInvoke :: Spec -> AppM Result
procInvoke (Spec cmd cmdArgs maybeWorkDir environ) = do
  homeDir <- getEnv "HOME"
  let environ' = ("HOME", homeDir) : environ
      process = (proc cmd cmdArgs) { cwd = maybeWorkDir, env = Just environ'
                                    , std_out = CreatePipe, std_err = CreatePipe }
  (_, stdOut, stdErr, procHandle) <- createProcess process
  code <- waitForProcess procHandle
  out <- liftIO (TIO.hGetContents (fromJust stdOut))
  err <- liftIO (TIO.hGetContents (fromJust stdErr))
  pure (Result code out err)

procEnsureSuccess :: Spec -> Result -> AppM ()
procEnsureSuccess spec (Result code out err) =
  case code of
    ExitSuccess -> pure ()
    ExitFailure failCode -> throwIO (ProcessFailedErr spec failCode out err)

procInvokeEnsure :: Spec -> AppM Result
procInvokeEnsure spec = do
  res <- procInvoke spec
  procEnsureSuccess spec res
  pure res

procInvokeEnsure_ :: Spec -> AppM ()
procInvokeEnsure_ = void . procInvokeEnsure

procDebug_ :: Spec -> AppM ()
procDebug_ spec@(Spec cmd cmdArgs maybeWorkDir environ) = do
  homeDir <- getEnv "HOME"
  let environ' = ("HOME", homeDir) : environ
      process = (proc cmd cmdArgs) { cwd = maybeWorkDir, env = Just environ'
                                    , std_out = Inherit, std_err = Inherit }
  (_, _, _, procHandle) <- createProcess process
  code <- waitForProcess procHandle
  case code of
    ExitSuccess -> pure ()
    ExitFailure failCode -> throwIO (ProcessFailedErr spec failCode "" "")
