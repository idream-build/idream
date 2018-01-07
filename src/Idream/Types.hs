
{-# LANGUAGE OverloadedStrings #-}

module Idream.Types ( PackageName(..)
                    , SourceDir(..)
                    , Repo(..)
                    , Version(..)
                    , Project(..)
                    , Package(..)
                    , PackageDescr(..)
                    , PackageSet(..)
                    ) where


-- Imports

import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import Data.Maybe (maybe)
import Control.Monad (mzero)


-- Data types

newtype PackageName = PackageName Text deriving (Eq, Show)
newtype SourceDir = SourceDir Text deriving (Eq, Show)
newtype Repo = Repo Text deriving (Eq, Show)
newtype Version = Version Text deriving (Eq, Show)

newtype Project = Project [PackageName] deriving (Eq, Show)
data Package = Library PackageName (Maybe SourceDir) [PackageName]
             | Executable PackageName (Maybe SourceDir) [PackageName]
data PackageDescr = PackageDescr Repo Version (Maybe SourceDir)
newtype PackageSet = PackageSet (Map Text PackageDescr)


-- Instances

instance FromJSON PackageName where
  parseJSON v = PackageName <$> parseJSON v

instance FromJSON SourceDir where
  parseJSON v = SourceDir <$> parseJSON v

instance FromJSON Repo where
  parseJSON v = Repo <$> parseJSON v

instance FromJSON Version where
  parseJSON v = Version <$> parseJSON v

instance FromJSON Project where
  parseJSON (Object o) = Project <$> o .: "packages"
  parseJSON _ = mzero

instance FromJSON Package where
  parseJSON (Object o) = do
    name <- o .: "name"
    srcDir <- o .: "sourcedir"
    pkgs <- o .:? "packages"
    case pkgs of
      Nothing -> do
        modules <- o .:? "modules"
        return $ Library name srcDir (maybe [] id modules)
      Just pkgs' -> return $ Executable name srcDir pkgs'
  parseJSON _ = mzero

instance FromJSON PackageDescr where
  parseJSON (Object o) =
    PackageDescr <$> o .: "repo"
                 <*> o .: "version"
                 <*> o .:? "subdir"
  parseJSON _ = mzero

instance FromJSON PackageSet where
  parseJSON v = PackageSet <$> parseJSON v
