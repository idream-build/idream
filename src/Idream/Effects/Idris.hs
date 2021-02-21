module Idream.Effects.Idris
  ( idrisCompile
  , idrisGetLibDir
  ) where

import qualified Data.Text as T
import Idream.App (AppM)
import Idream.Effects.Process (Result (..), Spec (..), procEnsureSuccess, procInvoke)
import Idream.FilePaths (Directory)
import Idream.Types (PackageName, ProjectName)

idrisGetLibDir :: AppM Directory
idrisGetLibDir = do
  let spec = Spec "idris2" ["--libdir"] Nothing []
  result <- procInvoke spec
  procEnsureSuccess spec result
  pure (T.unpack (T.stripEnd (resultOut result)))

idrisCompile :: ProjectName -> PackageName -> AppM ()
idrisCompile _projName _pkgName = error "TODO: idrisCompile"

-- runIdris :: forall e r. LastMember (SafeIO e) r
--          => (IdrisError -> e) -> Eff (Idris ': r) ~> Eff r
-- runIdris f = interpretM g where
--   g :: Idris ~> SafeIO e
--   g IdrisGetLibDir = do
--     let eh1 = f . IdrGetLibDirErr
--         eh2 ec err = f $ IdrInvokeErr "--libdir" [] ec err
--         idrisArgs = [ "--libdir" ]
--         toDir = filter (/= '\n')
--     toDir <$> invokeIdrisWithEnv eh1 eh2 idrisArgs Nothing []
--   g IdrisGetDocsDir = do
--     let eh1 = f . IdrGetDocsDirErr
--         eh2 ec err = f $ IdrInvokeErr "--docdir" [] ec err
--         idrisArgs = [ "--docdir" ]
--         toDir = filter (/= '\n')
--     toDir <$> invokeIdrisWithEnv eh1 eh2 idrisArgs Nothing []
--   g (IdrisCompile projName pkgName) = do
--     let eh1 = IdrCompileErr projName pkgName
--         idrisCompileArgs = [ "--verbose", "--build"]
--         idrisInstallArgs = [ "--verbose", "--install"]
--     void $ invokeIdrisForPkg f eh1 projName pkgName idrisCompileArgs "compile"
--     void $ invokeIdrisForPkg f eh1 projName pkgName idrisInstallArgs "install"
--   g (IdrisMkDocs projName pkgName) = do
--     let idrisMkDocsArgs = ["--verbose", "--mkdoc"]
--         idrisInstallDocsArgs = ["--verbose", "--installdoc"]
--         eh1 = IdrMkDocsErr projName pkgName
--     void $ invokeIdrisForPkg f eh1 projName pkgName idrisMkDocsArgs "mkdoc"
--     void $ invokeIdrisForPkg f eh1 projName pkgName idrisInstallDocsArgs "installdoc"
--   g (IdrisRepl projName pkgName) = do
--     let idrisReplArgs = [ "--verbose", "--repl"]
--         eh1 = IdrReplErr projName pkgName
--     void $ invokeIdrisForPkg f eh1 projName pkgName idrisReplArgs "repl"

-- -- | Converts a relative path into an absolute path.
-- absPath :: (IOException -> e) -> FilePath -> SafeIO e FilePath
-- absPath f path = liftSafeIO f $ makeAbsolute path
