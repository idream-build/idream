
module Idream.Command.Clean ( cleanCode ) where

-- Imports

import Control.Monad.Freer
import Control.Monad.Reader
import Idream.Effects.Log ( Logger, LogError, runLogger, logErr )
import Idream.Types ( Config(..), args, logLevel )
import qualified Idream.Effects.Log as Log
import Idream.Effects.FileSystem
import Idream.SafeIO
import Idream.ToText


-- Data Types

data CleanErr = CFSErr FSError
              | CLogErr LogError
              deriving (Eq, Show)


-- Instances

instance ToText CleanErr where
  toText (CFSErr e) = "Failed to clean project: " <> toText e <> "."
  toText (CLogErr e) = "Failed to clean project: " <> toText e <> "."


-- Functions

-- | Cleans up the working directory of a project.
cleanCode :: ( MonadReader Config m, MonadIO m ) => m ()
cleanCode = do
  result <- runProgram $ do
    Log.info "Cleaning project."
    removePath buildDir
  either (logErr . toText) return result

-- | Runs the actual program described in the Eff monad.
runProgram :: ( MonadReader Config m, MonadIO m )
           => Eff '[Logger, FileSystem, SafeIO CleanErr] ()
           -> m (Either CleanErr ())
runProgram prog = do
  thres <- asks $ logLevel . args
  liftIO $  runSafeIO
        <$> runM
         .  runFS CFSErr
         .  runLogger CLogErr thres
         $  prog

