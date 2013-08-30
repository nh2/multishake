#!/usr/bin/env runghc
{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable, NamedFieldPuns #-}

-- | multishake
--
-- A Shake build system that builds a cabal project together with a set of
-- dependencies ("extsrc", available as source code cabal packages).
--
-- The package must have a library and can have many executables.
--
-- Requires:
--   * shake
--   * cabal from https://github.com/nh2/cabal/tree/nh2work (for now)
--   * ghc-parmake
--   * posix-paths
--   * nproc (usually installed)
--   * hasktags (for tags)
module Main (main) where

-- base
import           Control.Applicative ((<$>))
import           Control.Monad
import           Data.List (foldl')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Language.Haskell.TH (location, loc_filename, Exp(LitE),  Lit (StringL))
import           Text.Read (readEither)
import           System.Console.GetOpt
import qualified System.Directory as IO
import           System.Environment (getArgs, withArgs)
import           System.Process (readProcess)

-- shake
import           Development.Shake
import           Development.Shake.Classes (hashWithSalt)
import           Development.Shake.FilePath

-- posix-paths
import System.Posix.Directory.Traversals


-- * CONFIGURATION

-- | "extsrc" -- the directory where the dependencies are (full of cabal packages).
_PACKAGES_DIR :: String -- with trailing backslash
_PACKAGES_DIR  = "../../extsrc/haskell/"

_CABAL_NAME, _CABAL_FILE :: String
_CABAL_NAME = "traderlib"       -- name of the main package
_CABAL_FILE = "traderlib.cabal" -- name of its cabal file

_TAGS_FILES :: [String]; _TAGS_ROOT, _TAGS_SCAN_DIRS :: String
_TAGS_FILES = ["../../tags", "../../TAGS"]        -- where to store tags
_TAGS_ROOT = "../../"                             -- from where to run hasktags
_TAGS_SCAN_DIRS = "src/trader/src extsrc/haskell" -- what to scan



-- * TODO

-- BuildOptions missing:
-- - OPTS
-- - CABAL_OPTS
-- - PROF
-- - CABALGEN_OPTS
--
-- add hscope



-- * EXTRA COMMAND LINE ARGUMENTS

data BuildOptions = BuildOptions
    { jobs :: Int
    , oLevel :: Int
    } deriving (Eq, Show)

defaultBuildOptions :: BuildOptions
defaultBuildOptions = BuildOptions
    { jobs = -1 -- set by `nproc`
    , oLevel = 2
    }

optDescrs :: [OptDescr (Either String (BuildOptions -> BuildOptions))]
optDescrs =
    [ Option "O" [] (optional "N"    (\o n -> o{ oLevel = n }) naturalIntArg) "optimization level to pass to GHC via -O"
    ]

-- | Helper to create an optional argument.
optional :: String -> (BuildOptions -> a -> BuildOptions) -> (String -> Either String a) -> ArgDescr (Either String (BuildOptions -> BuildOptions))
optional placeholder f eitherParser = OptArg g placeholder
    where
        g Nothing    = Right id
        g (Just str) = flip f <$> eitherParser str


-- | Parses an Int >= 0.
naturalIntArg :: String -> Either String Int
naturalIntArg s = case readEither s of
    Right n | n >= 0 -> Right n
    Right _          -> Left "argument must be >= 0"
    left             -> left

-- | Parses an Int > 0.
positiveIntArg :: String -> Either String Int
positiveIntArg s = case readEither s of
    Right n | n > 0 -> Right n
    Right _         -> Left "argument must be positive"
    left            -> left

-- | Errors on Left, prepending something to the error message.
fromEither :: String -> Either String a -> a
fromEither errPrepend e = either (error . (errPrepend ++)) id e


-- | Like `shakeArgsWith`, but for record style GetOpt options.
--  Remove when https://github.com/ndmitchell/shake/issues/50 is fixed.
shakeArgsWithRecord :: ShakeOptions -> [OptDescr (Either String (a -> a))] -> a -> (a -> [String] -> IO (Maybe (Rules ()))) -> IO ()
shakeArgsWithRecord opts ods def f = shakeArgsWith opts ods (\flagFuns targets -> f (foldl' (flip ($)) def flagFuns) targets)



-- * SHAKE STARTUP

-- | The name of this file we are in.
_THIS_FILE_NAME :: String
_THIS_FILE_NAME = $(LitE . StringL . loc_filename <$> location)


main :: IO ()
main = do
    -- Get a checksum of THIS FILE to use as shake version.clean-sreport
    -- This will trigger a rebuild when these rules are changed.
    checksum <- dropWhile (== '-') . show . hashWithSalt 0 <$> BS.readFile _THIS_FILE_NAME

    defaultJobs <- getCores
    j           <- hackShakeJobs defaultJobs

    -- NOTE: Shake (still as of 0.10.6) will cut off the version change
    --       message after `.` symbols (only in its display logic).
    --       See https://github.com/ndmitchell/shake/issues/39.
    let shakeOpts = shakeOptions
            { shakeLint = True
            , shakeVersion = "hash-" ++ checksum ++ "-" ++ _THIS_FILE_NAME
            -- , shakeReport = Just "_make/build-report.html"
            , shakeThreads = j
            }

    hackColors $ shakeArgsWithRecord shakeOpts optDescrs defaultBuildOptions $
        \buildOpts targets -> return $ Just $ rules targets buildOpts{ jobs = j }

    where
        -- Hack to use -j/--jobs for our own things AND for shake.
        -- We just run GetOpt on the shake default options.
        -- Remove when https://github.com/ndmitchell/shake/issues/51 is fixed.
        hackShakeJobs :: Int -> IO Int
        hackShakeJobs defaultJobs = do
            args <- getArgs
            let (flags, _, _errs) = getOpt Permute shakeOptDescrs args
                shakeOpts = foldl (flip ($)) shakeOptions{ shakeThreads = defaultJobs } $ map (either error id) flags
            -- Ignore _errs here because shakeOptDescrs doesn't expose things like --help
            return $ shakeThreads shakeOpts

        -- | Gets the number of CPUs via `nproc`. This is different from `numCapabilities`.
        getCores :: IO Int
        getCores = fromEither "nproc: " . positiveIntArg
                    <$> readProcess "nproc" [] ""

        -- Cheat in colors until https://github.com/ndmitchell/shake/issues/49
        hackColors f = do args <- getArgs
                          withArgs ("--color":args) f



-- * THE BUILD RULES

-- READ THIS for adding new rules:
--
-- * "Phony actions are intended to define command-line abbreviations.
--    You should not need phony actions as dependencies of rules [...]."
--   Means: Never `need ["myphony"]`, always `need ["file-needed-in-phony"]`.
--   There is also a bug that actually PREVENTS rebuilding in this case:
--     https://github.com/ndmitchell/shake/issues/55
rules :: [String] -> BuildOptions -> Rules ()
rules targets buildOptions@BuildOptions{ jobs, oLevel } = do

    -- Default target
    action $ do
        putNormal $ "Build options: " ++ show buildOptions

        -- Run actions that need to run first, e.g. such that clean the build.
        cleanOnOptimizationChange oLevel -- always check whether -O flags changed

        -- If no targets are given, run the ones we use most.
        -- TODO change this to the ones we ran last time?
        if targets /= []
            then need targets
            else need ["extsrc", "cabalbuild"]


    phony "help" $ do
        liftIO . putStrLn . unlines $
            [ "TODO: This is the help text" ]


    -- * ENVIRONMENT SECTION

    -- GHC version
    "_make/ghc/version" *> \out -> do
        alwaysRerun
        Stdout v <- cmd "ghc --numeric-version"
        writeFileChanged out v


    -- * CABAL SECTION

    phony "cabalgen" $ need [_CABAL_FILE]

    _CABAL_FILE *> \_ -> do
        alwaysRerun
        Stdout _ <- cmd "./cabalgen.py" -- silence -- TODO OPTS (-prof)
        return ()


    -- Available packages (ghc-pkg list inside cabal-dev)
    "_make/cabal-dev/pkglist" *> \out -> do
        alwaysRerun
        Stdout src <- cmd "cabal-dev ghc-pkg list -v"
        writeFileChanged out src


    -- cabal configure

    phony "configure" $ need ["_make/project/setup-config"]

    -- Runs cabal-dev configure to obtain the setup-config, fixing all dependency versions.
    "_make/project/setup-config" *> \setupConfig -> do
        -- We need extsrc ready for this so cabal configure can check the dependency versions.
        need [_CABAL_FILE, "_make/extsrc/packages-install-stamp"] -- TODO phony extsrc bug
        -- Depend on GHC version and all package hashes.
        -- Note that this breaks if you try to cheat packages in around cabal!
        need ["_make/ghc/version", "_make/cabal-dev/pkglist"]

        Stdout _ <- cmd "cabal-dev configure --disable-library-profiling" -- silence -- TODO CONFIG_OPTS

        -- cabal configure will always touch the setup-config.
        -- Copy if it to our own file if it has not changed such that it gets
        -- touched only when the contents actually changed.
        copyFileIfChanged "dist/setup-config" setupConfig


    -- cabal build

    phony "cabalbuild" $ need ["_make/project/build-stamp"]

    -- Runs cabal-dev build and touches the build-stamp on success.
    "_make/project/build-stamp" *> \stamp -> do
        -- We track extsrc, the cabal config and all source files used by cabal
        need ["_make/extsrc/packages-install-stamp", "_make/project/setup-config"] -- TODO phony extsrc bug
        need =<< allFillesIn ["src", "executables", "bench"]

        -- TODO all these OPTS
        --   cabal-dev build $(PARMAKE) --ghc-options="$(BUILD_OPTS) $(OPTS)" -j$(JOBS) $(CABAL_OPTS)
        () <- cmd "cabal-dev build" "--with-ghc=ghc-parmake" ["--ghc-options=-j " ++ show jobs ++ " -O" ++ show oLevel] ("lib:" ++ _CABAL_NAME)

        writeFile' stamp ""


    -- * CODE TOOLS SECTION

    -- Tags (hasktags)
    phony "tags" $ need _TAGS_FILES
    _TAGS_FILES *>> \_ -> do
        need =<< getDirectoryFiles "" ["//*.hs", dropTrailingPathSeparator _PACKAGES_DIR ++ "//*.hs"]
        cmd (Cwd _TAGS_ROOT) $ "hasktags --extendedctag " ++ _TAGS_SCAN_DIRS


    -- * EXTSRC SECTION
    --
    -- extsrc (external packages, available as source in the file system)

    -- Inspired by https://github.com/ndmitchell/shake/issues/38
    phony "extsrc" $ need ["_make/extsrc/packages-install-stamp"]

    -- Installs all cabal packages in _PACKAGES_DIR that have been changed.
    -- Time stamp file gets touched if something was installed.
    "_make/extsrc/packages-install-stamp" *> \stamp -> do
        -- Discover packages
        packages <- getDirectoryDirs _PACKAGES_DIR

        -- Which packages need rebuilding?
        toInstall <- flip filterM packages $ \p -> do
            need ["_make/extsrc" </> p <.> "stamp"]
            -- Use the non-shake version here to not require the .rebuild to
            -- exist in shake's lint (we create and delete it in one rule run).
            liftIO . IO.doesFileExist $ "_make/extsrc" </> p <.> "rebuild"

        -- Build/install packages
        liftIO . putStrLn . unlines $ ["Building with cabal:"] ++ map ("  - " ++) toInstall
        cabalDevInstall jobs [ _PACKAGES_DIR </> p | p <- toInstall ]

        -- Mark packages as installed
        forM_ toInstall $ \p ->
            liftIO . IO.removeFile $ "_make/extsrc/" </> p <.> "rebuild"

        -- Update stamp to make clear we installed something
        writeFile' stamp ""

    -- For each package, make a composite time stamp (package.stamp).
    "_make/extsrc/*.stamp" *> \stamp -> do
        packageFiles <- readFileLines $ stamp -<.> "source-files"
        need packageFiles
        -- If any file changed, mark package as needing a rebuild
        writeFile' (stamp -<.> "rebuild") ""
        writeFile' stamp ""

    -- For each package, creates a list of files we watch for changes (package.source-files).
    "_make/extsrc/*.source-files" *> \sf -> do
        let packageName = takeBaseName sf
        packageFiles <- liftIO $ getCabalPackageFiles packageName
        writeFileChanged sf $ unlines packageFiles


    -- * CLEANING SECTION
    --
    -- Note that removing target files has no further effect and will not
    -- result in them being rebuilt.
    -- For that, files Shake knows about have to be missing/touched.

    phony "clean" $ do

        putQuiet . red $ "Cleaning project"

        liftIO $ removeFiles "." [ "dist//**"             -- remove build files of this project
                                 , _CABAL_FILE            -- remove ./cabalgen.py generated cabal file
                                 , "gen-executables//*" ] -- remove ./cabalgen.py executable stubs
        -- TODO
        -- cmd "rm -f .last_opt_level"
        -- cmd "make -C ../../web clean"
        liftIO $ removeFiles "." ["_make/project//*"]

    phony "clean-extsrc" $ do

        putQuiet . red $ "Cleaning extsrc"

        -- Discover packages
        packages <- getDirectoryDirs _PACKAGES_DIR

        -- Remove build files in all packages
        forM_ packages $ \p -> do
            let packageDir = _PACKAGES_DIR </> p
            -- We remove dist/ instead of using cabal clean
            whenExists (packageDir </> "dist") $
                liftIO $ removeFiles packageDir ["dist//*"]

    phony "clean-all" $ do
        need ["clean", "clean-extsrc"]
        liftIO $ removeFiles "." [ "_make//*"
                                 , "cabal-dev//*" ]



-- * HELPERS

-- | Checks whether the -O optimization level has changed, and if yes, calls clean.
-- This must be done BEFORE any other rules are run!
cleanOnOptimizationChange :: Int -> Action ()
cleanOnOptimizationChange newOLevel = do

    let lastOFile = "_make/last-O" -- stores the last used -O level

    exists <- liftIO $ IO.doesFileExist lastOFile
    when exists $ do
        lastOLevel <- fromEither (lastOFile ++ ": ") . naturalIntArg
                      <$> liftIO (readFile lastOFile)

        -- When the optimization level changes, we need to rebuild.
        -- We don't need to rebuild extsrc because we always build that with -O2.
        when (newOLevel /= lastOLevel) $ do
            putQuiet . red $ "Optimization level changed to " ++ show newOLevel
            need ["clean"]

    writeFileChanged lastOFile (show newOLevel ++ "\n")


-- | Uses `cabal-dev install` to install the packages in the given directories.
-- The directories MUST contain `.cabal` files!
cabalDevInstall :: Int -> [String] -> Action ()
cabalDevInstall _ [] = return () -- nothing to do if no paths given
cabalDevInstall nJobs packagePaths
    = cmd (EchoStdout True)
          "cabal-dev install --sandbox=cabal-dev --force-reinstalls"
          "--enable-library-profiling --disable-documentation"
          ("-j" ++ show nJobs)
          -- Append trailing slashes to make sure cabal treats it as
          -- directories instead of package names
          (map addTrailingPathSeparator packagePaths)


-- | Print something in red.
red :: String -> String
red msg = escape "31" msg
    where
        escape code x = "\ESC[" ++ code ++ "m" ++ x ++ "\ESC[0m"


-- TODO Ask for shake inclusion
getDirectoryFilesPrefixed :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFilesPrefixed dir pattern = map (dir </>) <$> getDirectoryFiles dir pattern


-- TODO Ask for shake inclusion
copyFileIfChanged :: FilePath -> FilePath -> Action ()
copyFileIfChanged src dest = readFile' src >>= writeFileChanged dest


-- | Gets all files (no directories) in any of the given directories.
-- Uses Shake's file tracking.
allFillesIn :: [FilePath] -> Action [FilePath]
allFillesIn dirs = concat <$>
    (forM dirs $ \dir -> getDirectoryFilesPrefixed dir ["//*"])


-- | Runs the action if the file/directory exists.
-- Uses Shake's file tracking.
whenExists :: FilePath -> Action () -> Action ()
whenExists path act = do
    file <- doesFileExist path
    dir  <- doesDirectoryExist path
    when (dir || file) act


-- | Lists all files (no directories) in a given root + subdirectories.
findFiles :: FilePath -> IO [FilePath]
findFiles root = allDirectoryContents (BS8.pack root)
                 >>= filterM IO.doesFileExist . map BS8.unpack


-- | Gets all files of a cabal project in _PACKAGES_DIR, excluding dist/ files.
-- Example:  getCabalPackageFiles "webserver-0.7.1.0"
getCabalPackageFiles :: String -> IO [FilePath]
getCabalPackageFiles p = do
    let packageRoot = _PACKAGES_DIR </> p
        relative path = makeRelative packageRoot path -- path of file from package root

    -- Use non-Shake file list function to not track the files we want to exclude.
    packageFiles <- liftIO $ findFiles packageRoot

    return [ f | f <- packageFiles, takeDirectory1 (relative f) /= "dist" ]
