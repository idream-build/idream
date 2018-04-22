
module Idream.Command.New ( startNewProject ) where


-- Imports

import Prelude hiding ( writeFile )
import Control.Monad.Freer
import Control.Monad.Reader
import System.FilePath ( (</>) )
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import qualified Data.Text as T
import Idream.Types ( ProjectName(..), Config(..), logLevel, args )
import Idream.FileSystem
import Idream.Log ( Logger, LogError, logErr )
import qualified Idream.Log as Log
import Idream.SafeIO


-- Data types

-- | Custom error type for when creating a project template.
data MkProjectError = MPLogErr LogError
                    | MPFSErr FSError
                    deriving (Eq, Show)


-- Functions

gitignore, idrPkgSetJson :: Text
idrProjectJson :: Text -> Text

gitignore = ".idream-work/\n"
idrPkgSetJson = "{}\n"
idrProjectJson projName =
  T.unlines [ "{"
            , "    \"project_name\": \"" <> projName <> "\","
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
  either showError return result

runProgram :: ( MonadReader Config m, MonadIO m )
           => Eff '[Logger, FileSystem, SafeIO MkProjectError] ()
           -> m (Either MkProjectError ())
runProgram prog = do
  thres <- asks $ logLevel . args
  liftIO $ runSafeIO
        <$> runM
         . runFS MPFSErr
         . Log.runLogger MPLogErr thres $ prog

-- | Does the actual creation of the project template.
startNewProject' :: ( Member Logger r, Member FileSystem r )
                 => ProjectName -> Eff r ()
startNewProject' (ProjectName projName) = do
  createDir projectDir
  createDir $ relPath buildDir
  writeFile gitignore $ relPath ".gitignore"
  writeFile (idrProjectJson projName) (relPath projectFile)
  writeFile idrPkgSetJson $ relPath pkgSetFile
  Log.info ("Successfully initialized project: " <> projName <> ".")
  where projectDir = T.unpack projName
        relPath path = projectDir </> path

-- | Displays the error if one occurred during project template creation.
showError :: MonadIO m => MkProjectError -> m ()
showError (MPFSErr e) =
  logErr ("Failed to initialize project: " <> T.pack (show e))
showError (MPLogErr e) =
  logErr ("Failed to initialize project: " <> T.pack (show e))

