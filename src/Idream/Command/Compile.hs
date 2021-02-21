module Idream.Command.Compile where
-- module Idream.Command.Compile ( compileCode ) where

-- -- Imports

-- import Control.Monad.Freer
-- import Control.Monad.Reader
-- import qualified Data.Text as T
-- import Idream.Command.Common (ProjParseErr (..), readRootProjFile)
-- import Idream.Effects.FileSystem
-- import Idream.Effects.Idris
-- import Idream.Effects.Log (Logger)
-- import qualified Idream.Effects.Log as Log
-- import Idream.Error
-- import Idream.Graph (BuildPlan, DepNode (..), ParseGraphErr, createBuildPlan, loadGraphFromJSON)
-- import Idream.SafeIO
-- import Idream.ToText
-- import Idream.Types (Config (..), Project (..), args, logLevel)
-- import System.FilePath ((</>))


-- -- Data types

-- data CompileErr = CFSErr FSError
--                 | CLogErr Log.LogError
--                 | CIdrisErr IdrisError
--                 | CProjParseErr ProjParseErr
--                 | CGraphErr ParseGraphErr
--                 deriving (Eq, Show)

-- -- Instances

-- instance ToText CompileErr where
--   toText (CFSErr err) = toText err
--   toText (CLogErr err) = toText err
--   toText (CIdrisErr err) = toText err
--   toText (CProjParseErr err) = toText err
--   toText (CGraphErr err) = toText err


-- -- Functions

-- -- | Top level function for compiling Idris packages.
-- compileCode :: ( MonadReader Config m, MonadIO m ) => m ()
-- compileCode = do
--   result <- runProgram $ do
--     Project _ rootPkgs <- readRootProjFile
--     if null rootPkgs
--       then Log.info ("Project contains no packages yet, skipping compile step. "
--                   <> "Use `idream add` to add a package to this project first.")
--       else do
--         Log.info "Compiling package(s)..."
--         graph <- loadGraphFromJSON depGraphFile
--         let buildPlan = createBuildPlan graph
--         compilePackages buildPlan
--         Log.info "Successfully compiled package(s)!"
--   case result of
--     Left err -> Log.logErr $ toText err
--     Right _ -> return ()

-- -- | Performs the actual compilation of the Idris packages.
-- --   Takes the buildplan into account to properly build dependencies in order.
-- compilePackages :: ( Member Logger r, Member Idris r, Member FileSystem r )
--                 => BuildPlan DepNode -> Eff r ()
-- compilePackages buildPlan = do
--   libDir <- idrisGetLibDir
--   createDir compileDir
--   Log.debug "Copying files from base packages into compile directory."
--   setupBasePackages libDir
--   mapM_ compilePackage buildPlan  -- TODO optimize / parallellize, handle deps...
--   where compilePackage (DepNode pkgName projName) = do
--           Log.debug ("Compiling package: " <> toText pkgName <> ".")
--           idrisCompile projName pkgName
--           Log.info ("Compiled package: " <> toText pkgName <> ".")

-- -- Copies over the 'standard' packages used by Idris into the main compilation directory.
-- setupBasePackages :: Member FileSystem r => Directory -> Eff r ()
-- setupBasePackages libDir = mapM_ setupBasePackage basePackages where
--   basePackages = ["base", "contrib", "network", "prelude"]
--   setupBasePackage pkg = do
--     let fromDir = libDir </> T.unpack pkg
--     copyDir fromDir compileDir

-- -- | Helper function that runs the actual program (described using Eff monad).
-- runProgram :: ( MonadReader Config m, MonadIO m )
--            => Eff '[ Logger
--                    , Error CompileErr, Error ProjParseErr, Error ParseGraphErr
--                    , Idris, FileSystem, SafeIO CompileErr ] ()
--            -> m (Either CompileErr ())
-- runProgram prog = do
--   thres <- asks $ logLevel . args
--   liftIO $  fmap (join . join . join)
--          $  runSafeIO
--         <$> runM
--          .  runFS CFSErr
--          .  runIdris CIdrisErr
--          .  runError' CGraphErr
--          .  runError' CProjParseErr
--          .  runError
--          .  Log.runLogger CLogErr thres
--          $  prog

