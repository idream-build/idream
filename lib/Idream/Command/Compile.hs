
module Idream.Command.Compile ( compileCode ) where

-- Imports

import Control.Monad.Reader
import Control.Monad.Freer
import Idream.Error
import Idream.SafeIO
import Idream.Effects.FileSystem
import Idream.Effects.Idris
import Idream.Effects.Log ( Logger )
import Idream.ToText
import qualified Idream.Effects.Log as Log
import Idream.Types ( Config(..), Project(..), logLevel, args )
import Idream.Graph ( DepNode(..), BuildPlan, ParseGraphErr
                    , loadGraphFromJSON, createBuildPlan )
import Idream.Command.Common ( readRootProjFile, ProjParseErr(..) )
import Data.Monoid


-- Data types

data CompileErr = CFSErr FSError
                | CLogErr Log.LogError
                | CIdrisErr IdrisError
                | CProjParseErr ProjParseErr
                | CGraphErr ParseGraphErr
                deriving (Eq, Show)

-- Instances

instance ToText CompileErr where
  toText (CFSErr err) = toText err
  toText (CLogErr err) = toText err
  toText (CIdrisErr err) = toText err
  toText (CProjParseErr err) = toText err
  toText (CGraphErr err) = toText err


-- Functions


compileCode :: ( MonadReader Config m, MonadIO m ) => m ()
compileCode = do
  result <- runProgram $ do
    Project _ rootPkgs <- readRootProjFile
    if null rootPkgs
      then Log.info ("Project contains no packages yet, skipping compile step. "
                  <> "Use `idream add` to add a package to this project first.")
      else do
        Log.info "Compiling package(s)..."
        graph <- loadGraphFromJSON depGraphFile
        let buildPlan = createBuildPlan graph
        compilePackages buildPlan
        Log.info "Successfully compiled package(s)!"
  case result of
    Left err -> Log.logErr $ toText err
    Right _ -> return ()

compilePackages :: ( Member Logger r, Member Idris r, Member FileSystem r )
                => BuildPlan DepNode -> Eff r ()
compilePackages = mapM_ compilePackage where
  -- TODO optimize / parallellize, handle deps...
  compilePackage (DepNode pkgName projName) = do
    Log.debug ("Compiling package: " <> toText pkgName)
    let compileDir = pkgCompileDir projName pkgName
    createDir compileDir
    idrisCompile projName pkgName
    Log.info ("Compiled package: " <> toText pkgName <> ".")

-- | Helper function that runs the actual program (described using Eff monad).
runProgram :: ( MonadReader Config m, MonadIO m )
           => Eff '[ Logger
                   , Error CompileErr, Error ProjParseErr, Error ParseGraphErr
                   , Idris, FileSystem, SafeIO CompileErr] ()
           -> m (Either CompileErr ())
runProgram prog = do
  thres <- asks $ logLevel . args
  liftIO $  fmap (join . join . join)
         $  runSafeIO
        <$> runM
         .  runFS CFSErr
         .  runIdris CIdrisErr
         .  runError' CGraphErr
         .  runError' CProjParseErr
         .  runError
         .  Log.runLogger CLogErr thres
         $  prog

