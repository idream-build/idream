
module Idream.Command.GenerateIpkg ( generateIpkgFile ) where


-- Imports

import Prelude hiding ( writeFile )
import Control.Monad.Reader
import Control.Monad.Freer
import Idream.Error
import Idream.Effects.Log ( Logger, LogError, logErr )
import Idream.SafeIO
import qualified Idream.Effects.Log as Log
import Idream.Types
import Idream.Graph
import Idream.ToText
import qualified Algebra.Graph as Graph
import Idream.Command.Common ( ProjParseErr(..), PkgParseErr(..)
                             , readRootProjFile, readPkgFile
                             , getPkgFilePath, getPkgDirPath )
import Idream.Effects.FileSystem
import Data.Text ( Text )
import Data.Monoid ( (<>) )
import Data.List ( intercalate )
import qualified Data.Text as T
import System.FilePath.Posix


-- Data types

-- | Type alias for modules inside an ipkg file.
type Module = FilePath

-- | Data type containing all info needed to generate an ipkg file.
data IpkgMetadata = IpkgMetadata Package [Module]
  deriving (Eq, Show)

-- | Error type used for storing all errors when generating .ipkg files.
data GenerateIpkgErr = GFSErr FSError
                     | GLogErr LogError
                     | GRootProjParseErr ProjParseErr
                     | GProjParseErr ProjParseErr
                     | GPkgParseErr PkgParseErr
                     | GGraphErr ParseGraphErr
                     deriving (Eq, Show)

-- Instances

instance ToText GenerateIpkgErr where
  toText (GFSErr err) = toText err
  toText (GLogErr err) = toText err
  toText (GGraphErr err) = toText err
  toText (GRootProjParseErr err) = "Failed to parse root project file: " <> toText err
  toText (GProjParseErr err) = toText err
  toText (GPkgParseErr err) = toText err


-- Functions

-- | Top level function used for generating .ipkg files.
generateIpkgFile :: ( MonadReader Config m, MonadIO m ) => m ()
generateIpkgFile = do
  result <- runProgram $ do
    Project _ rootPkgs <- readRootProjFile
    if null rootPkgs
      then Log.info ("Project contains no packages yet, skipping generate step. "
                  <> "Use `idream add` to add a package to this project first.")
      else do
        Log.info "Generating .ipkg files..."
        graph <- loadGraphFromJSON depGraphFile
        mapM_ generateIpkg $ Graph.vertexList graph
        Log.info "Finished generating .ipkg files."
  either (logErr . toText) return result

-- | Generates an ipkg file for 1 package in a project.
--   Note that this also cleans the build directory for that project.
generateIpkg :: ( Member Logger r
                , Member (Error ProjParseErr) r
                , Member (Error PkgParseErr) r
                , Member FileSystem r )
             => DepNode -> Eff r ()
generateIpkg node@(DepNode pkgName projName) = do
  Log.debug ("Generating ipkg file for package: " <> toText pkgName <> ".")
  pkgDirPath <- getPkgDirPath pkgName projName
  let projectBuildDir' = projectBuildDir projName
      pkgBuildDir' = pkgBuildDir projName pkgName
  removePath pkgBuildDir'
  createDir pkgBuildDir'
  copyDir pkgDirPath projectBuildDir'
  generateIpkgHelper node

-- | Helper function that does the actual generation of the .ipkg file.
generateIpkgHelper :: ( Member Logger r
                      , Member (Error ProjParseErr) r
                      , Member (Error PkgParseErr) r
                      , Member FileSystem r )
                   => DepNode -> Eff r ()
generateIpkgHelper (DepNode pkgName projName) = do
  pkgFilePath <- getPkgFilePath pkgName projName
  package@(Package _ _ srcDir _) <- readPkgFile pkgFilePath
  let pkgBuildSrcDir' = pkgBuildSrcDir projName pkgName srcDir
  idrisFiles <- findFiles (hasExt "idr") (Just pkgBuildSrcDir')
  let pkgMetadata = IpkgMetadata package idrisFiles
      contents = ipkgMetadataToText pkgMetadata
      ipkg = ipkgFile projName pkgName
  Log.debug ("Writing .ipkg file to: " <> toText ipkg)
  writeFile contents ipkg

hasExt :: String -> FilePath -> Bool
hasExt ext fp =
  let (_, ext') = splitExtension fp
   in ext' == "." <> ext

-- | Converts the ipkg metadata to a text representation.
ipkgMetadataToText :: IpkgMetadata -> Text
ipkgMetadataToText (IpkgMetadata pkg modules) =
  let (Package pkgName pkgType (SourceDir srcDir) dependencies) = pkg
      name = unPkgName pkgName
      mods = T.pack $ intercalate ", " $ formatFileNames modules
      deps = T.intercalate ", " $ toText <$> dependencies
      sourceDir = T.pack srcDir
  in T.unlines [ "package " <> name
               , "-- NOTE: This is an auto-generated file by idream. Do not edit."
               , "modules = " <> mods
               , if null dependencies then "" else "pkgs = " <> deps
               , "sourcedir = " <> sourceDir
               , if pkgType == Executable then "executable = " <> name else ""
               , if pkgType == Executable then "main = Main" else ""
               ]

-- | Formats the filename for use in ipkg file.
--   e.g. ./LightYear/Position.idr -> LightYear.Position
formatFileNames :: [String] -> [String]
formatFileNames modules = replaceSlashes . trimLeadingSlash . trimExt <$> modules
  where replaceSlash '/' = '.'
        replaceSlash c = c
        trimLeadingSlash = drop 2
        trimExt s = take (length s - 4) s
        replaceSlashes = fmap replaceSlash

-- | Helper function for running the program described in the Eff monad.
runProgram :: ( MonadReader Config m, MonadIO m )
           => Eff '[ Logger
                   , Error PkgParseErr, Error ProjParseErr, Error ParseGraphErr
                   , FileSystem, SafeIO GenerateIpkgErr] ()
           -> m (Either GenerateIpkgErr ())
runProgram prog = do
  thres <- asks $ logLevel . args
  liftIO $  fmap (join . join . join)
         $  runSafeIO
        <$> runM
         .  runFS GFSErr
         .  runError' GGraphErr
         .  runError' GProjParseErr
         .  runError' GPkgParseErr
         .  Log.runLogger GLogErr thres
         $  prog

