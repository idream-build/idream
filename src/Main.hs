
{-# LANGUAGE OverloadedStrings#-}

module Main where

-- Imports

import Data.Default (def)
import Data.Aeson (eitherDecode)
import Idream.OptionParser
import Idream.Types
import System.IO.Error
import qualified Data.ByteString.Lazy as B


-- Data types

data Config = Config { args :: Args
                     , buildSettings :: BuildSettings
                     } deriving (Eq, Show)


-- Constants

buildSettingsFile :: String
buildSettingsFile = ".idream"


-- Functions


readBuildSettings :: IO BuildSettings
readBuildSettings = flip catchIOError (const def) $ do
  jsonContents <- B.readFile buildSettingsFile
  either (const def) (return) $ eitherDecode jsonContents

main :: IO ()
main = do
  settings <- readBuildSettings
  cmdLineArgs <- parseCmdLineArgs
  let config = Config cmdLineArgs settings
  putStrLn $ show config
