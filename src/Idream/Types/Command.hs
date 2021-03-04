module Idream.Types.Command
  ( Argument
  , Command (..)
  , Args (..)
  ) where

import qualified Data.Text as T
import Idream.Prelude
import Idream.Types.Common (PackageGroup, PackageName, PackageType, ProjectName (..), RefreshStrategy)
import LittleLogger (Severity)

-- | Type alias for command line arguments passed to the run command.
type Argument = Text

-- | Type describing the various commands that can be passed in via the commandline interface.
data Command
  = Fetch PackageGroup RefreshStrategy  -- ^ Fetches all dependencies as described in json file
  | Compile PackageGroup        -- ^ Compiles all code
  | Clean                       -- ^ Cleans up build artifacts and fetched code
  | Run PackageName [Argument]  -- ^ Runs an executable
  | Repl PackageName            -- ^ Opens up the repl
  | New ProjectName             -- ^ Initializes a new project for use with idream
  | Add (Maybe Directory) PackageName PackageType  -- ^ Adds a package to an existing idream project
  | Test PackageGroup           -- ^ Runs tests for this project
  deriving (Eq, Show)

-- | Data structure representing the arguments passed in to the program.
data Args = Args
  { argsSeverity :: Severity
  , argsProjDir :: Maybe Directory
  , argsCommand :: Command
  } deriving (Eq, Show)
