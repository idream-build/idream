
module Idream.Command.Repl ( startRepl ) where


import Control.Monad.Reader
import Control.Monad.Freer
import Idream.Error
import Idream.SafeIO
import Idream.Effects.Idris
import Idream.Effects.Log ( Logger )
import Idream.ToText
import qualified Idream.Effects.Log as Log
import Idream.Types ( Config, ProjectName, PackageName, logLevel, args )


data ReplErr = RLogErr Log.LogError
             | RIdrisErr IdrisError
             deriving (Eq, Show)


instance ToText ReplErr where
    toText (RLogErr err) = toText err
    toText (RIdrisErr err) = toText err


startRepl :: ( MonadReader Config m, MonadIO m ) => ProjectName -> PackageName -> m ()
startRepl projName pkgName = do
  result <- runProgram $ do
    idrisRepl projName pkgName
  case result of
    Left err -> Log.logErr $ toText err
    Right _ -> return ()


runProgram :: ( MonadReader Config m, MonadIO m )
    => Eff '[ Logger, Error ReplErr
            , Idris, SafeIO ReplErr] ()
    -> m (Either ReplErr ())
runProgram prog = do
    thres <- asks $ logLevel . args
    liftIO $  fmap join
           $  runSafeIO
          <$> runM
           .  runIdris RIdrisErr
           .  runError
           .  Log.runLogger RLogErr thres
           $  prog
