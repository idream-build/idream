
module Idream.Command.Clean ( cleanCode ) where

-- Imports

import Control.Monad.Freer
import Control.Monad.Reader
import Idream.Log ( Logger, LogError, runLogger, logErr )
import Idream.Types ( Config(..), args, logLevel )
import qualified Idream.Log as Log
import Idream.SafeIO
import Idream.FileSystem
import qualified Data.Text as T
import Data.Monoid ( (<>) )


-- Data Types

data CleanErr = CFSErr FSError
              | CLogErr LogError
              deriving (Eq, Show)


-- Functions

-- | Cleans up the working directory of a project.
cleanCode :: ( MonadReader Config m, MonadIO m ) => m ()
cleanCode = do
  result <- runProgram $ do
    Log.info "Cleaning project."
    removePath buildDir
  either showError return result

runProgram :: ( MonadReader Config m, MonadIO m )
           => Eff '[Logger, FileSystem, SafeIO CleanErr] ()
           -> m (Either CleanErr ())
runProgram prog = do
  thres <- asks $ logLevel . args
  liftIO $ runSafeIO
        <$> runM
         .  runFS CFSErr
         .  runLogger CLogErr thres
         $  prog

-- | Displays the error if something went wrong during project cleanup.
showError :: MonadIO m => CleanErr -> m ()
showError (CFSErr e) =
  logErr ("Failed to clean project: " <> T.pack (show e))
showError (CLogErr e) =
  logErr ("Failed to clean project: " <> T.pack (show e))


