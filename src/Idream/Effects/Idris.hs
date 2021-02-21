module Idream.Effects.Idris
  ( idrisCompile
  , idrisGetLibDir
  ) where

import Idream.App (AppM)
import Idream.FilePaths (Directory)
import Idream.Types (PackageName, ProjectName)

idrisGetLibDir :: AppM Directory
idrisGetLibDir = error "TODO: idrisGetLibDir"

idrisCompile :: ProjectName -> PackageName -> AppM ()
idrisCompile _projName _pkgName = error "TODO: idrisCompile"
