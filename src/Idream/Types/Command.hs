module Idream.Types.Command
  ( Argument
  , Command (..)
  , Args (..)
  ) where

import Data.Text (Text)
import Idream.Types.Common (PackageName, PackageType, ProjectName)
import LittleLogger (Severity)

-- | Type alias for command line arguments passed to the run command.
type Argument = Text

-- | Type describing the various commands that can be passed in via the commandline interface.
data Command
  = Fetch                        -- ^ Fetches all dependencies as described in json file
  | Compile                      -- ^ Compiles all code (fetch needs to be done first to get dependencies code?)
  | Clean                        -- ^ Cleans up build artifacts, fetched code?
  | Run [Argument]               -- ^ Runs the executable defined in idr-project.json
  | Repl ProjectName PackageName  -- ^ Opens up the repl
  | New ProjectName              -- ^ Initializes a new project for use with idream
  | Add PackageName PackageType  -- ^ Adds a package to an existing idream project
  | MkDoc                        -- ^ Generates the documentation
  | GenerateIpkg                 -- ^ Generates a ipkg file from the idream JSON files
  | Test                         -- ^ Runs unit tests for this project
  deriving (Eq, Show)

-- | Data structure representing the arguments passed in to the program.
data Args = Args
  { argsSeverity :: Severity
  , argsCommand :: Command
  } deriving (Eq, Show)
