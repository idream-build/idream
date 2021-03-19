-- Based on https://github.com/idris-lang/Idris-dev/blob/master/src/Idris/Package/Parser.hs
-- And https://github.com/idris-lang/Idris2/blob/master/src/Idris/Package.idr

{-# LANGUAGE FlexibleContexts #-}
module Idream.IpkgParser
  ( Parser
  , runParserFile
  , runParserPure
  , parsePackageVersion
  , parsePackageDesc
  ) where

import qualified Data.Text.IO as TIO
import Idream.Prelude
import Idream.Types.Ipkg (PackageDesc (..), PackageVersion (..))
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL

type Parser a = Parsec Void Text a

runParserFile :: Parser a -> FilePath -> AppM a
runParserFile p fp = do
  contents <- liftIO (TIO.readFile fp)
  either throwIO pure (MP.parse p fp contents)

runParserPure :: Parser a -> Text -> Maybe a
runParserPure = MP.parseMaybe

parsePackageVersion :: Parser PackageVersion
parsePackageVersion = fmap PackageVersion (MP.sepBy MPCL.decimal (MPC.char '.'))

parsePackageDesc :: Parser PackageDesc
parsePackageDesc = undefined
