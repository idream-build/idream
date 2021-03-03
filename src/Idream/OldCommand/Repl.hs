module Idream.OldCommand.Repl where
-- module Idream.Command.Repl ( startRepl ) where


-- import Control.Monad.Freer
-- import Control.Monad.Reader
-- import Idream.Command.Common (ProjParseErr (..), readRootProjFile)
-- import Idream.Effects.FileSystem
-- import Idream.Effects.Idris
-- import Idream.Effects.Log (Logger)
-- import qualified Idream.Effects.Log as Log
-- import Idream.Error
-- import Idream.SafeIO
-- import Idream.ToText
-- import Idream.Types (Config, PackageName, Project (..), ProjectName, args, logLevel)


-- data ReplErr = RLogErr Log.LogError
--              | RProjParseErr ProjParseErr
--              | RFSErr FSError
--              | RIdrisErr IdrisError
--              deriving (Eq, Show)


-- instance ToText ReplErr where
--     toText (RLogErr err) = toText err
--     toText (RFSErr err) = toText err
--     toText (RProjParseErr err) = toText err
--     toText (RIdrisErr err) = toText err


-- startRepl :: ( MonadReader Config m, MonadIO m )
--           => ProjectName -> PackageName -> m ()
-- startRepl projName pkgName = do
--   result <- runProgram $ do
--     -- TODO check needed if packages compiled yet?
--     Project _ rootPkgs <- readRootProjFile
--     if null rootPkgs
--       then Log.info ("Project contains no packages yet, skipping REPL step. "
--                   <> "Use `idream add` to add a package to this project first. "
--                   <> "Afterwards, compile your packages using `idream compile` before using the REPL.")
--       else idrisRepl projName pkgName
--   case result of
--     Left err -> Log.logErr $ toText err
--     Right _ -> return ()


-- runProgram :: ( MonadReader Config m, MonadIO m )
--            => Eff '[ Logger
--                    , Error ReplErr, Error ProjParseErr
--                    , Idris, FileSystem, SafeIO ReplErr] ()
--            -> m (Either ReplErr ())
-- runProgram prog = do
--   thres <- asks $ logLevel . args
--   liftIO $  fmap (join . join)
--          $  runSafeIO
--         <$> runM
--          .  runFS RFSErr
--          .  runIdris RIdrisErr
--          .  runError' RProjParseErr
--          .  runError
--          .  Log.runLogger RLogErr thres
--          $  prog
