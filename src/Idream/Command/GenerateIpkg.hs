module Idream.Command.GenerateIpkg where
--   ( generateIpkgFile
--   ) where

-- import qualified Algebra.Graph as Graph
-- import Data.List (intercalate, stripPrefix)
-- import Data.Maybe (fromJust)
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import Idream.App (AppM)
-- import Idream.Command.Common (getIpkgFilePath, getPkgFilePath, readPkgFile, readRootProjFile)
-- import Idream.Effects.FileSystem (fsFindFiles, fsWriteFile)
-- import Idream.FileLogic (depGraphFile)
-- import Idream.FilePaths (Directory, hasExt)
-- import Idream.Graph (DepNode (..), loadGraphFromJSON)
-- import Idream.ToText (ToText (..))
-- import Idream.Types (Package (..), PackageType (..), Project (..), ProjectName, SourceDir (..))
-- import LittleLogger (logDebug, logInfo)

-- -- | Type alias for modules inside an ipkg file.
-- type Module = FilePath

-- -- | Data type containing all info needed to generate an ipkg file.
-- data IpkgMetadata = IpkgMetadata ProjectName Package [Module]
--   deriving (Eq, Show)

-- -- | Top level function used for generating .ipkg files.
-- generateIpkgFile :: AppM ()
-- generateIpkgFile = do
--   Project _ rootPkgs <- readRootProjFile
--   if null rootPkgs
--     then logInfo ("Project contains no packages yet, skipping generate step. "
--                  <> "Use `idream add` to add a package to this project first.")
--     else do
--       logInfo "Generating .ipkg files..."
--       graph <- loadGraphFromJSON depGraphFile
--       mapM_ generateIpkg $ Graph.vertexList graph
--       logInfo "Finished generating .ipkg files."

-- -- | Generates an ipkg file for 1 package in a project.
-- --   Note that this also cleans the build directory for that project.
-- generateIpkg :: DepNode -> AppM ()
-- generateIpkg (DepNode pkgName projName) = do
--   logDebug ("Generating ipkg file for package: " <> toText pkgName <> ".")
--   ipkg <- getIpkgFilePath pkgName projName
--   pkgFilePath <- getPkgFilePath pkgName projName
--   package@(Package _ _ srcDir _) <- readPkgFile pkgFilePath

--   idrisFiles <- fsFindFiles (hasExt "idr") (Just srcDir)
--   let pkgMetadata = IpkgMetadata projName package idrisFiles
--       contents = ipkgMetadataToText pkgMetadata
--   logDebug ("Writing .ipkg file to: " <> toText ipkg)
--   fsWriteFile ipkg contents

-- -- | Converts the ipkg metadata to a text representation.
-- ipkgMetadataToText :: IpkgMetadata -> Text
-- ipkgMetadataToText (IpkgMetadata projName pkg modules) =
--   let (Package pkgName pkgType (SourceDir srcDir) dependencies) = pkg
--       fullPkgName = toText projName <> "_" <> toText pkgName
--       exeName = toText pkgName
--       mods = T.pack $ intercalate ", " $ formatFileNames srcDir modules
--       deps = T.intercalate ", " $ toText <$> dependencies
--       sourceDir = T.pack srcDir
--   in T.unlines [ "package " <> fullPkgName
--                , "-- NOTE: This is an auto-generated file by idream. Do not edit."
--                , "modules = " <> mods
--                , if null dependencies then "" else "pkgs = " <> deps
--                , "sourcedir = " <> sourceDir
--                , if pkgType == Executable then "executable = " <> exeName else ""
--                , if pkgType == Executable then "main = Main" else ""
--                ]

-- -- | Formats the filename for use in ipkg file.
-- --   e.g. ./LightYear/Position.idr -> LightYear.Position
-- formatFileNames :: Directory -> [String] -> [String]
-- formatFileNames pkgBuildSrcDir' modules =
--   replaceSlashes . trimPrefix . trimExt <$> modules
--   where replaceSlash '/' = '.'
--         replaceSlash c = c
--         trimPrefix = fromJust . stripPrefix (pkgBuildSrcDir' ++ "/")
--         trimExt s = take (length s - 4) s
--         replaceSlashes = fmap replaceSlash
