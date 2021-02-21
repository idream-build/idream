module Idream.Effects.Git where
-- module Idream.Effects.Git ( Git(..)
--                           , GitError(..), GitCloneError(..), GitCheckoutError(..)
--                           , runGit, gitClone, gitCheckout
--                           ) where


-- -- Imports

-- import Control.Exception (IOException)
-- import Control.Monad (when)
-- import Control.Monad.Freer
-- import qualified Data.Text as T
-- import Idream.Effects.FileSystem
-- import Idream.SafeIO
-- import Idream.ToText
-- import Idream.Types (Repo (..), Version (..))
-- import System.Exit (ExitCode (..))
-- import System.Process (StdStream (CreatePipe), createProcess, cwd, proc, std_err, waitForProcess)

-- -- Data types

-- type Command = String

-- type Argument = String

-- data Git a where
--   GitClone :: Repo -> Directory -> Git ()
--   GitCheckout :: Repo -> Version -> Directory -> Git ()

-- data GitError = GitCloneErr GitCloneError
--               | GitCheckoutErr GitCheckoutError
--               deriving (Eq, Show)

-- -- | Error type used for describing errors that can occur during clone of dependencies.
-- data GitCloneError = CloneFailError Repo IOException
--                    | CloneFailExitCode Repo ExitCode
--                    deriving (Eq, Show)

-- -- | Error type used for describing errors that can occur during checkout of dependencies.
-- data GitCheckoutError = CheckoutFailError Repo Version IOException
--                       | CheckoutFailExitCode Repo Version ExitCode
--                       deriving (Eq, Show)

-- -- Instances

-- instance ToText GitError where
--   toText (GitCloneErr err) = toText err
--   toText (GitCheckoutErr err) = toText err

-- instance ToText GitCloneError where
--   toText (CloneFailError repo err) =
--     "Failed to git clone " <> toText repo <> ", reason: " <> toText err <> "."
--   toText (CloneFailExitCode repo ec) =
--     "`git clone` for repo " <> toText repo <> " returned non-zero exit code: "
--       <> toText ec <> "."

-- instance ToText GitCheckoutError where
--   toText (CheckoutFailError repo vsn err) =
--     "Failed to do git checkout for " <> toText repo
--       <> ", version = " <> toText vsn <> ", reason: " <> toText err <> "."
--   toText (CheckoutFailExitCode repo vsn ec) =
--     "git checkout for repo " <> toText repo
--       <> ", version =" <> toText vsn <> " returned non-zero exit code: "
--       <> toText ec <> "."


-- -- Functions

-- gitClone :: Member Git r => Repo -> Directory -> Eff r ()
-- gitClone repo dir = send $ GitClone repo dir

-- gitCheckout :: Member Git r => Repo -> Version -> Directory -> Eff r ()
-- gitCheckout repo vsn dir = send $ GitCheckout repo vsn dir

-- runGit :: forall e r. LastMember (SafeIO e) r
--        => (GitError -> e) -> Eff (Git ': r) ~> Eff r
-- runGit f = interpretM g where
--   g :: Git ~> SafeIO e
--   g (GitClone repo dir) = gitCloneHelper f repo dir
--   g (GitCheckout repo vsn dir) = gitCheckoutHelper f repo vsn dir

-- execProcess :: (IOException -> e)
--             -> Command -> [Argument] -> Maybe Directory
--             -> SafeIO e ExitCode
-- execProcess f command cmdArgs maybeDir = liftSafeIO f $ do
--   (_, _, _, procHandle) <- createProcess (proc command cmdArgs) { cwd = maybeDir, std_err = CreatePipe }
--   waitForProcess procHandle

-- gitCloneHelper :: (GitError -> e)
--                -> Repo -> Directory -> SafeIO e ()
-- gitCloneHelper f repo@(Repo r) downloadDir = do
--   let cloneArgs = ["clone", "--quiet", "--recurse-submodules", T.unpack r, downloadDir]
--   result <- mapError (f . GitCloneErr) $ execProcess (CloneFailError repo) "git" cloneArgs Nothing
--   when (result /= ExitSuccess) $ raiseError . f . GitCloneErr $ CloneFailExitCode repo result

-- gitCheckoutHelper :: (GitError -> e)
--                   -> Repo -> Version -> Directory -> SafeIO e ()
-- gitCheckoutHelper f repo v@(Version vsn) downloadDir = do
--   let checkoutArgs = ["checkout", "--quiet", T.unpack vsn]
--   result <- mapError (f . GitCheckoutErr)
--           $ execProcess (CheckoutFailError repo v) "git" checkoutArgs (Just downloadDir)
--   when (result /= ExitSuccess) $ raiseError . f . GitCheckoutErr $ CheckoutFailExitCode repo v result

