{-# LANGUAGE FlexibleInstances #-}

module Idream.ToText
  ( ToText(..)
  ) where

import Control.Exception (IOException)
import qualified Data.Text as T
import System.Exit (ExitCode)

-- | Helper typeclass for converting a type to a text representation.
-- TODO(ejconlon) Consider text-show and a separate ExceptionInfo class?
class ToText a where
  toText :: a -> T.Text

instance ToText T.Text where
  toText = id

instance ToText String where
  toText = T.pack

instance ToText IOException where
  toText = T.pack . show

instance ToText ExitCode where
  toText = T.pack . show
