
module Idream.Command.Add ( addPackageToProject ) where


-- Imports

import Prelude hiding ( writeFile )
import Control.Monad.Reader
import Control.Monad.Freer
import Idream.Error
import Idream.SafeIO
import qualified Idream.Effects.Log as Log
import Idream.Effects.Log ( Logger )
import Idream.ToText
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import Data.Text.Lazy.Encoding ( decodeUtf8 )
import Data.Aeson.Encode.Pretty ( encodePretty )
import Data.Aeson.Types ( ToJSON(..) )
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import System.FilePath ( (</>) )
import Idream.Effects.FileSystem
import Idream.Types ( Project(..), PackageName(..), PackageType(..), Config(..)
                    , logLevel, args )
import Idream.Command.Common ( readRootProjFile, ProjParseErr(..) )


-- Data types

-- | Custom error type for when creating a project template.
data AddPkgError = PackageAlreadyExistsErr PackageName
                 | AProjParseErr ProjParseErr
                 | AFSErr FSError
                 | ALogErr Log.LogError
                 deriving (Eq, Show)

-- Instances

instance ToText AddPkgError where
  toText (AFSErr err) = toText err
  toText (ALogErr err) = toText err
  toText (AProjParseErr err) = toText err
  toText (PackageAlreadyExistsErr pkgName) =
    "Failed to add package to project, package " <> toText pkgName <> " already exists."


-- Functions

libIdr, mainIdr :: Text
idrPkgJson :: PackageName -> PackageType -> Text

libIdr =
  T.unlines [ "module Lib"
            , ""
            , "||| Library function, to be replaced with actual code."
            , "libFunction : String"
            , "libFunction = \"Hello, Idris!\""
            ]
mainIdr =
  T.unlines [ "module Main"
            , ""
            , "||| Main program, to be replaced with actual code."
            , "main : IO ()"
            , "main = putStrLn \"Hello, Idris!\""
            ]
idrPkgJson (PackageName pkgName) pkgType =
  T.unlines [ "{"
            , "    \"name\": \"" <> pkgName <> "\","
            , "    \"source_dir\": \"src\","
            , if pkgType == Library
              then "    \"executable\": false,"
              else "    \"executable\": true,"
            , "    \"dependencies\": []"
            , "}"
            ]

-- | Creates a new project template.
addPackageToProject :: ( MonadReader Config m, MonadIO m )
                    => PackageName -> PackageType -> m ()
addPackageToProject pkgName@(PackageName name) pkgType = do
  result <- runProgram $ do
    pkgDirExists <- doesDirExist (T.unpack name)
    if pkgDirExists
       then Log.err $ toText $ PackageAlreadyExistsErr pkgName
      else addPackageToProject' pkgName pkgType
  either (Log.logErr . toText) return result

-- | Does the actual creation of the project template.
addPackageToProject' :: ( Member Logger r
                        , Member (Error ProjParseErr) r
                        , Member (Error AddPkgError) r
                        , Member FileSystem r )
                     => PackageName -> PackageType -> Eff r ()
addPackageToProject' pkgName pkgType = do
  projInfo <- readRootProjFile
  createDir $ pkgDir pkgName
  createDir $ pkgSrcDir pkgName
  writeFile (idrPkgJson pkgName pkgType) (pkgDir pkgName </> pkgFile)
  writeFile (mainContents pkgType) (pkgSrcDir pkgName </> mainFile pkgType)
  updateProjInfo projInfo pkgName
  Log.info ("Successfully added package " <> toText pkgName <> " to project.")
  where mainFile Library = "Lib.idr"
        mainFile Executable = "Main.idr"
        mainContents Library = libIdr
        mainContents Executable = mainIdr

-- | Updates the project file with a new package entry.
updateProjInfo :: Member FileSystem r => Project -> PackageName -> Eff r ()
updateProjInfo projInfo pkgName =
  let projFilePath = projectFile
      updatedDeps = projDeps projInfo ++ [pkgName]
      projInfo' = projInfo { projDeps = updatedDeps }
   in writeFile (encodeJSON projInfo') projFilePath

-- | Encodes a serializable JSON value to pretty printed text.
encodeJSON :: ToJSON a => a -> Text
encodeJSON x = (TL.toStrict . decodeUtf8 . encodePretty $ x) <> "\n"

-- | Runs the actual program described in the Eff monad.
runProgram :: ( MonadReader Config m, MonadIO m )
           => Eff '[ Logger
                   , Error AddPkgError, Error ProjParseErr
                   , FileSystem, SafeIO AddPkgError] ()
           -> m (Either AddPkgError ())
runProgram prog = do
  thres <- asks $ logLevel . args
  liftIO $  fmap (join . join)
         $  runSafeIO
        <$> runM
         .  runFS AFSErr
         .  runError' AProjParseErr
         .  runError
         .  Log.runLogger ALogErr thres
         $  prog

