module Idream.OldEffects.Idris where
-- module Idream.Effects.Idris ( Idris(..), IdrisError(..)
--                             , Command, Arg, Environment
--                             , idrisGetLibDir
--                             , idrisGetDocsDir
--                             , idrisCompile
--                             , idrisRepl
--                             , idrisMkDocs
--                             , runIdris
--                             ) where

-- -- Imports

-- import Control.Exception (IOException)
-- import Control.Monad (void)
-- import Control.Monad.Freer
-- import Data.Maybe (fromJust)
-- import qualified Data.Text as T
-- import GHC.IO.Handle (hGetContents)
-- import Idream.FilePaths
-- import Idream.SafeIO
-- import Idream.ToText
-- import Idream.Types (PackageName (..), ProjectName (..))
-- import System.Directory (makeAbsolute)
-- import System.Environment (getEnv)
-- import System.Exit (ExitCode (..))
-- import System.Process (StdStream (CreatePipe), createProcess, cwd, env, proc, std_err, std_out, waitForProcess)


-- -- Data types

-- data Idris a where
--   IdrisGetLibDir :: Idris Directory
--   IdrisGetDocsDir :: Idris Directory
--   IdrisCompile :: ProjectName -> PackageName -> Idris ()
--   IdrisRepl :: ProjectName -> PackageName -> Idris ()
--   IdrisMkDocs :: ProjectName -> PackageName -> Idris ()

-- data IdrisError = IdrGetLibDirErr IOException
--                 | IdrGetDocsDirErr IOException
--                 | IdrCompileErr ProjectName PackageName IOException
--                 | IdrReplErr ProjectName PackageName IOException
--                 | IdrMkDocsErr ProjectName PackageName IOException
--                 | IdrInvokeErr Command [Arg] ExitCode String
--                 | IdrAbsPathErr IOException
--                 deriving (Eq, Show)


-- -- | Type alias for command when spawning an external OS process.
-- type Command = String

-- -- | Type alias for command line arguments when spawning an external OS process.
-- type Arg = String

-- -- | Type alias for an environment to be passed to a command,
-- --   expressed as a list of key value pairs.
-- type Environment = [(String, String)]


-- -- Instances

-- instance ToText IdrisError where
--   toText (IdrGetLibDirErr err) =
--     "Failed to get lib directory for Idris packages, reason: "
--       <> toText err <> "."
--   toText (IdrGetDocsDirErr err) =
--     "Failed to get docs directory for Idris packages, reason: "
--       <> toText err <> "."
--   toText (IdrCompileErr projName pkgName err) =
--     "Failed to compile idris package (project = "
--       <> toText projName <> ", package = " <> toText pkgName
--       <> "), reason: " <> toText err <> "."
--   toText (IdrReplErr projName pkgName err) =
--     "Failed to start REPL for idris package (project = "
--       <> toText projName <> ", package = " <> toText pkgName
--       <> "), reason: " <> toText err <> "."
--   toText (IdrMkDocsErr projName pkgName err) =
--     "Failed to generate docs for idris package (project = "
--       <> toText projName <> ", package = " <> toText pkgName
--       <> "), reason: " <> toText err <> "."
--   toText (IdrInvokeErr cmd args exitCode errOutput) =
--     "Failed to invoke idris (command: " <> toText cmd
--       <> ", args: " <> toText (unwords args)
--       <> "), exit code = " <> T.pack (show exitCode)
--       <> ", error output:\n" <> toText errOutput
--   toText (IdrAbsPathErr err) =
--     "Failed to compute absolute file path, reason: " <> toText err


-- -- Functions

-- idrisGetLibDir :: Member Idris r => Eff r Directory
-- idrisGetLibDir = send IdrisGetLibDir

-- idrisGetDocsDir :: Member Idris r => Eff r Directory
-- idrisGetDocsDir = send IdrisGetDocsDir

-- idrisCompile :: Member Idris r
--              => ProjectName -> PackageName -> Eff r ()
-- idrisCompile projName pkgName = send $ IdrisCompile projName pkgName

-- idrisMkDocs :: Member Idris r
--             => ProjectName -> PackageName -> Eff r ()
-- idrisMkDocs projName pkgName = send $ IdrisMkDocs projName pkgName

-- idrisRepl :: Member Idris r
--           => ProjectName -> PackageName -> Eff r ()
-- idrisRepl projName pkgName = send $ IdrisRepl projName pkgName

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

-- -- | Invokes a command as a separate operating system process.
-- --   Allows passing additional environment variables to the external process.
-- invokeCmdWithEnv :: (IOException -> e)
--                  -> (ExitCode -> String -> e)
--                  -> Command -> [Arg] -> Maybe Directory -> Environment
--                  -> SafeIO e String
-- invokeCmdWithEnv f g cmd cmdArgs maybeWorkDir environ = do
--   result <- liftSafeIO f $ do
--     homeDir <- getEnv "HOME"
--     let environ' = ("HOME", homeDir) : environ
--         process = (proc cmd cmdArgs) { cwd = maybeWorkDir, env = Just environ'
--                                      , std_out = CreatePipe, std_err = CreatePipe }
--     (_, stdOut, stdErr, procHandle) <- createProcess process
--     result <- waitForProcess procHandle
--     if result /= ExitSuccess
--       then do output <- hGetContents $ fromJust stdOut
--               errOutput <- hGetContents $ fromJust stdErr
--               return $ Left (g result $ output ++ errOutput)
--       else do output <- hGetContents $ fromJust stdOut
--               return $ Right output
--   either raiseError return result

-- -- | Invokes the Idris compiler as a separate operating system process.
-- --   Allows passing additional environment variables to the external process.
-- invokeIdrisWithEnv :: (IOException -> e)
--                    -> (ExitCode -> String -> e)
--                    -> [Arg] -> Maybe Directory -> Environment
--                    -> SafeIO e String
-- invokeIdrisWithEnv f g = invokeCmdWithEnv f g "idris2"

-- -- | Helper function for invoking idris using the ipkg file
-- --   for a specific project / package with certain command line arguments.
-- invokeIdrisForPkg :: forall e. (IdrisError -> e)
--                   -> (IOException -> IdrisError)
--                   -> ProjectName -> PackageName
--                   -> [Arg] -> Command
--                   -> SafeIO e String
-- invokeIdrisForPkg f eh1 projName pkgName args cmd = do
--   absDocsDir <- absPath (f . IdrAbsPathErr) docsDir
--   absCompileDir <- absPath (f . IdrAbsPathErr) compileDir
--   let buildDir' = pkgBuildDir projName pkgName
--       ipkg = fromJust $ ipkgFile projName pkgName `relativeTo` buildDir'
--       args' = args ++ [ipkg]
--       environ = [ ("IDRIS_LIBRARY_PATH", absCompileDir)
--                 , ("IDRIS_DOC_PATH", absDocsDir) ]
--       eh2 ec err = f $ IdrInvokeErr cmd args' ec err
--   invokeIdrisWithEnv (f . eh1) eh2 args' (Just buildDir') environ

