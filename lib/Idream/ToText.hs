
module Idream.ToText ( ToText(..) ) where

-- Imports

import qualified Data.Text as T
import Control.Exception ( IOException )
import System.Exit ( ExitCode )


-- Typeclasses

-- | Helper typeclass for converting a type to a text representation.
class ToText a where
  toText :: a -> T.Text


-- Instances

instance ToText T.Text where
  toText = id

instance ToText String where
  toText = T.pack

instance ToText IOException where
  toText = T.pack . show

instance ToText ExitCode where
  toText = T.pack . show

