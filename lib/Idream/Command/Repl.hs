
module Idream.Command.Repl ( startRepl ) where


import Control.Monad.Reader
import Control.Monad.Freer
import Idream.Error
import Idream.SafeIO
import Idream.Effects.Idris
import Idream.Effects.FileSystem
import Idream.Effects.Log ( Logger )
import Idream.ToText
import qualified Idream.Effects.Log as Log
import Idream.Types ( Config, Project(..), PackageName, logLevel, args )
import Idream.Command.Common ( readRootProjFile, ProjParseErr(..) )
import Data.Monoid ((<>))


data ReplErr = RLogErr Log.LogError
             | RProjParseErr ProjParseErr
             | RFSErr FSError
             | RIdrisErr IdrisError
             deriving (Eq, Show)


instance ToText ReplErr where
    toText (RLogErr err) = toText err
    toText (RFSErr err) = toText err
    toText (RProjParseErr err) = toText err
    toText (RIdrisErr err) = toText err


startRepl :: ( MonadReader Config m, MonadIO m )
          => PackageName -> m ()
startRepl pkgName = do
  result <- runProgram $ do
    -- TODO check needed if packages compiled yet?
    Project projName rootPkgs <- readRootProjFile
    if null rootPkgs
      then Log.info ("Project contains no packages yet, skipping REPL step. "
                  <> "Use `idream add` to add a package to this project first. "
                  <> "Afterwards, compile your packages using `idream compile` before using the REPL.")
      else idrisRepl projName pkgName
  case result of
    Left err -> Log.logErr $ toText err
    Right _ -> return ()


runProgram :: ( MonadReader Config m, MonadIO m )
           => Eff '[ Logger
                   , Error ReplErr, Error ProjParseErr
                   , Idris, FileSystem, SafeIO ReplErr] ()
           -> m (Either ReplErr ())
runProgram prog = do
  thres <- asks $ logLevel . args
  liftIO $  fmap (join . join)
         $  runSafeIO
        <$> runM
         .  runFS RFSErr
         .  runIdris RIdrisErr
         .  runError' RProjParseErr
         .  runError
         .  Log.runLogger RLogErr thres
         $  prog

