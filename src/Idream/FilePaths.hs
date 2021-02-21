-- | Generic path manipulation functions.
module Idream.FilePaths
  ( Directory
  , hasExt
  , relativeTo
  ) where

import Data.List (stripPrefix)
import System.FilePath (splitExtension)

-- | Type alias for directories.
type Directory = FilePath

-- | Checks if the given filepath has a specific extension.
hasExt :: String -> FilePath -> Bool
hasExt ext fp =
  let (_, ext') = splitExtension fp
   in ext' == "." <> ext

-- | Helper function for returning file name relative to a directory,
--   if the file is not contained in the current directory, it returns Nothing.
relativeTo :: FilePath -> Directory -> Maybe FilePath
relativeTo path dir = stripPrefix (dir ++ "/") path
