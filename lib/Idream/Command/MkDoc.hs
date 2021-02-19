
module Idream.Command.MkDoc ( generateDocs ) where

-- Imports

import Control.Monad.Reader
import Control.Monad.Freer
import Idream.SafeIO
import Idream.Error
import Idream.Effects.Idris
import Idream.Effects.FileSystem
import Idream.Effects.Log ( Logger )
import qualified Idream.Effects.Log as Log
import Idream.Command.Common ( readRootProjFile, ProjParseErr(..) )
import Idream.Types ( Project(..), Config(..), logLevel )
import Idream.ToText
import Idream.Graph
import qualified Data.Text as T
import System.FilePath ( (</>) )


-- Data types

data MkDocsErr = MFSErr FSError
               | MIdrisErr IdrisError
               | MLogErr Log.LogError
               | MProjParseErr ProjParseErr
               | MGraphErr ParseGraphErr
               deriving (Eq, Show)

instance ToText MkDocsErr where
  toText (MFSErr err) = toText err
  toText (MIdrisErr err) = toText err
  toText (MLogErr err) = toText err
  toText (MProjParseErr err) = toText err
  toText (MGraphErr err) = toText err


-- Funtions

generateDocs :: ( MonadReader Config m, MonadIO m ) => m ()
generateDocs = do
  result <- runProgram $ do
    Project _ rootPkgs <- readRootProjFile
    if null rootPkgs
      then Log.info ("Project contains no packages yet, skipping generation of docs."
                  <> "Use `idream add` to add a package to this project first.")
      else do
        Log.info "Generating documentation for package(s)..."
        graph <- loadGraphFromJSON depGraphFile
        let buildPlan = createBuildPlan graph
        genDocs buildPlan
        Log.info "Successfully generated documentation for package(s)!"
  case result of
    Left err -> Log.logErr $ toText err
    Right _ -> return ()

-- | Performs the actual generation of docs for the Idris packages.
--   Takes the buildplan into account to properly build dependencies in order.
genDocs :: ( Member Logger r, Member FileSystem r, Member Idris r)
        => BuildPlan DepNode -> Eff r ()
genDocs buildPlan = do
  docDir <- idrisGetDocsDir
  createDir docsDir
  Log.debug "Copying documentation files from base packages into docs directory."
  setupDocsForBasePackages docDir
  mapM_ genDoc buildPlan  -- TODO optimize / parallellize
    where genDoc (DepNode pkgName projName) = do
            Log.debug ("Generating documentation for package: " <> toText pkgName <> ".")
            idrisMkDocs projName pkgName
            Log.info ("Generated documentation for package: " <> toText pkgName <> ".")

-- | Copies over the documentation for the 'standard' packages used by Idris
--   into the main documentation directory.
setupDocsForBasePackages :: Member FileSystem r => Directory -> Eff r ()
setupDocsForBasePackages docDir = mapM_ setupDocsForBasePackage basePackages where
  basePackages = ["base", "contrib", "effects", "prelude", "pruviloj"]
  setupDocsForBasePackage pkg = do
    let fromDir = docDir </> T.unpack pkg
    copyDir fromDir docsDir

-- | Helper function that runs the actual program (described in the Eff monad).
runProgram :: ( MonadReader Config m, MonadIO m )
           => Eff '[ Logger
                   , Error MkDocsErr, Error ProjParseErr, Error ParseGraphErr
                   , Idris, FileSystem, SafeIO MkDocsErr ] ()
           -> m (Either MkDocsErr ())
runProgram prog = do
  thres <- asks $ logLevel . args
  liftIO $  fmap (join . join . join)
         $  runSafeIO
        <$> runM
         .  runFS MFSErr
         .  runIdris MIdrisErr
         .  runError' MGraphErr
         .  runError' MProjParseErr
         .  runError
         .  Log.runLogger MLogErr thres
         $  prog

