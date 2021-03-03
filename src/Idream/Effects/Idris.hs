module Idream.Effects.Idris
  ( idrisGetLibDir
  ) where

import qualified Data.Text as T
import Idream.App (AppM)
import Idream.Effects.Process (Result (..), Spec (..), procInvokeEnsure)
import Idream.FilePaths (Directory)
import Idream.Types.Common (PackageName, ProjectName)

idrisGetLibDir :: AppM Directory
idrisGetLibDir = do
  let spec = Spec "idris2" ["--libdir"] Nothing []
  result <- procInvokeEnsure spec
  pure (T.unpack (T.stripEnd (resultOut result)))
