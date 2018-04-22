
module Idream.Command.Compile ( compileCode ) where

-- Imports

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Reader
import Idream.SafeIO
import Idream.Effects.FileSystem
import Idream.Effects.Idris
import Idream.Effects.Log ( Logger )
import Idream.ToText
import qualified Idream.Effects.Log as Log
import Idream.Types ( Config(..), PackageName(..), Project(..), logLevel, args )
import Idream.Graph ( DepNode(..), BuildPlan, ParseGraphErr
                    , loadGraphFromJSON, createBuildPlan )
import Idream.Command.Common ( readRootProjFile, ProjParseErr(..) )
import qualified Data.Text as T
import Data.Monoid


-- Data types

data CompileErr = CFSErr FSError
                | CLogErr Log.LogError
                | CIdrisErr IdrisError
                | CProjReadFileErr ProjParseErr
                | CLoadGraphErr ParseGraphErr
                deriving (Eq, Show)


-- Functions


runProgram :: ( MonadReader Config m, MonadIO m )
           => Eff '[Logger, Error CompileErr, Idris, FileSystem, SafeIO CompileErr] ()
           -> m (Either CompileErr ())
runProgram prog = do
  thres <- asks $ logLevel . args
  liftIO $  fmap join
         $  runSafeIO
        <$> runM
         .  runFS CFSErr
         .  runIdris CIdrisErr
         .  runError
         .  Log.runLogger CLogErr thres
         $ prog

compileCode :: ( MonadReader Config m, MonadIO m ) => m ()
compileCode = do
  result <- runProgram $ do
    Project _ rootPkgs <- readRootProjFile CProjReadFileErr
    if null rootPkgs
      then Log.info ("Project contains no packages yet, skipping compile step. "
                  <> "Use `idream add` to add a package to this project first.")
      else do
        Log.info "Compiling package(s)..."
        graph <- loadGraphFromJSON CLoadGraphErr depGraphFile
        let buildPlan = createBuildPlan graph
        compilePackages buildPlan
        Log.info "Successfully compiled package(s)!"
  case result of
    Left err -> Log.logErr $ handleCompileErr err
    Right _ -> return ()

compilePackages :: ( Member Logger r, Member Idris r, Member FileSystem r )
                => BuildPlan DepNode -> Eff r ()
compilePackages = mapM_ compilePackage where
  -- TODO optimize / parallellize, handle deps...
  compilePackage depNode@(DepNode (PackageName pkgName) _) = do
    Log.debug ("Compiling package: " <> pkgName)
    idrisCompile' depNode
    Log.info ("Compiled package: " <> pkgName <> ".")


idrisCompile' :: ( Member FileSystem r, Member Idris r)
              => DepNode -> Eff r ()
idrisCompile' (DepNode pkgName projName) = do
  let compileDir = pkgCompileDir projName pkgName
      ipkg = ipkgFile projName pkgName
      idrisArgs = [ "--verbose", "--build", ipkg]
      environ = [ ("IDRIS_LIBRARY_PATH", compileDir)
                , ("IDRIS_DOC_PATH", pkgDocsDir projName pkgName)
                ]
  createDir compileDir
  idrisCompile projName pkgName idrisArgs environ


handleCompileErr :: CompileErr -> T.Text
handleCompileErr (CFSErr err) = toText err
handleCompileErr (CLogErr err) = toText err
handleCompileErr (CIdrisErr err) = toText err
handleCompileErr (CProjReadFileErr err) = toText err
handleCompileErr (CLoadGraphErr err) = toText err

