
module Idream.Command.New ( startNewProject ) where


-- Imports

import Prelude hiding ( writeFile )
import Control.Monad.Freer
import Control.Monad.Reader
import System.FilePath ( (</>) )
import Data.Text ( Text )
import qualified Data.Text as T
import Idream.Types ( ProjectName(..), Config(..), logLevel, args )
import Idream.Effects.FileSystem
import Idream.Effects.Log ( Logger, LogError, logErr )
import qualified Idream.Effects.Log as Log
import Idream.SafeIO
import Idream.ToText


-- Data types

-- | Custom error type for when creating a project template.
data MkProjectError = MPLogErr LogError
                    | MPFSErr FSError
                    deriving (Eq, Show)

-- Instances

instance ToText MkProjectError where
  toText (MPFSErr e) = "Failed to initialize project: " <> toText e
  toText (MPLogErr e) = "Failed to initialize project: " <> toText e


-- Functions

gitignore, idrPkgSetJson :: Text
idrProjectJson :: ProjectName -> Text

gitignore = ".idream-work/\n"
idrPkgSetJson = "{}\n"
idrProjectJson projName =
  T.unlines [ "{"
            , "    \"project_name\": \"" <> toText projName <> "\","
            , "    \"packages\": ["
            , ""
            , "    ]"
            , "}"
            , ""
            ]

-- | Creates a new project template.
startNewProject :: ( MonadReader Config m, MonadIO m ) => ProjectName -> m ()
startNewProject projName = do
  result <- runProgram (startNewProject' projName)
  either (logErr . toText) return result

-- | Does the actual creation of the project template.
startNewProject' :: ( Member Logger r, Member FileSystem r )
                 => ProjectName -> Eff r ()
startNewProject' projName = do
  createDir projectDir
  createDir $ relPath buildDir
  writeFile gitignore $ relPath ".gitignore"
  writeFile (idrProjectJson projName) (relPath projectFile)
  writeFile idrPkgSetJson $ relPath pkgSetFile
  Log.info ("Successfully initialized project: " <> toText projName <> ".")
  where projectDir = T.unpack $ toText projName
        relPath path = projectDir </> path

-- | Helper function that runs the actual program described in the Eff monad.
runProgram :: ( MonadReader Config m, MonadIO m )
           => Eff '[Logger, FileSystem, SafeIO MkProjectError] ()
           -> m (Either MkProjectError ())
runProgram prog = do
  thres <- asks $ logLevel . args
  liftIO $  runSafeIO
        <$> runM
         .  runFS MPFSErr
         .  Log.runLogger MPLogErr thres $ prog

