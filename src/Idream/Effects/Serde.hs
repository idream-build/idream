module Idream.Effects.Serde
  ( serdeReadJSON
  , serdeWriteJSON
  ) where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import qualified Data.ByteString.Lazy as BSL
import Idream.Effects.FileSystem (ReadFileErr (..), WriteFileErr (..))
import Idream.Prelude

serdeReadJSON :: (Exception e, FromJSON a) => (String -> e) -> FilePath -> AppM a
serdeReadJSON conv path = do
  ep <- catchIO (liftIO (eitherDecodeFileStrict' path)) (throwIO . ReadFileErr path)
  case ep of
    Left err -> throwIO (conv err)
    Right val -> pure val

prettyConfig :: Config
prettyConfig = defConfig { confIndent = Spaces 2, confTrailingNewline = True }

serdeWriteJSON :: ToJSON a => FilePath -> a -> AppM ()
serdeWriteJSON path val = do
  let pretty = encodePretty' prettyConfig val
  catchIO (liftIO (BSL.writeFile path pretty)) (throwIO . WriteFileErr path)
