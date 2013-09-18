#!/usr/bin/env runghc
{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}

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
module Main where

-- base
import           Control.Applicative ((<$>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List (foldl', isPrefixOf, sort)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Language.Haskell.TH (location, loc_filename, Exp(LitE),  Lit (StringL))
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Time (diffUTCTime, getCurrentTime)
import           Text.Read (readEither)
import           System.Console.GetOpt
import           System.Directory (createDirectoryIfMissing)
import qualified System.Directory as IO
import           System.Environment (getArgs, withArgs, lookupEnv)
import           System.Exit (ExitCode(..), exitWith)
import           System.Posix.ByteString.FilePath (RawFilePath)
import           System.Posix.Files.ByteString (getFileStatus, isDirectory)
import           System.Process (readProcessWithExitCode, system)

-- shake
import           Development.Shake
import           Development.Shake.Classes (hashWithSalt)
import           Development.Shake.FilePath

-- posix-paths
import System.Posix.Directory.Traversals (allDirectoryContents)


-- * CONFIGURATION

-- | Where in the repo the build file is.
-- For convenience so that the build file can be called from anywhere.
_BUILD_TOP_LEVEL_DIR :: FilePath
_BUILD_TOP_LEVEL_DIR = "src/myproject"

-- | Changing of which files shall trigger cabal rebuilding?
_CABAL_BUILD_SOURCE_FILES :: IO [String]
_CABAL_BUILD_SOURCE_FILES = allFilesIn ["src", "executables", "bench"]

-- | "extsrc" -- the directory where the dependencies are (full of cabal packages).
_PACKAGES_DIR :: String -- with trailing backslash
_PACKAGES_DIR  = "../../extsrc/haskell/"

_CABAL_NAME, _CABAL_FILE :: String
_CABAL_NAME = "myproject"       -- name of the main package
_CABAL_FILE = "myproject.cabal" -- name of its cabal file

_TAGS_FILES :: [String]; _TAGS_ROOT, _TAGS_SCAN_DIRS :: String
_TAGS_FILES = ["../../tags", "../../TAGS"]        -- where to store tags
_TAGS_ROOT = "../../"                             -- from where to run hasktags
_TAGS_SCAN_DIRS = "src/myproject/src extsrc/haskell" -- what to scan (from _TAGS_ROOT)



-- * BUILD GROUPS

-- | Plain short cuts for a group of executables.
-- Expanded to targets before anything is run.
-- Groups can only expand to real executables, not other groups.
-- We also depend on install for all groups.
exeGroup :: String -> [String]
exeGroup "somegroup"      = "install" : ["exe1", "exe2"]
exeGroup target           = [target] -- Any other target just stays a normal target. Important!

-- | Some cabal targets have exceptions: They need more than only
-- having an executable of that name build by cabal, and have their
-- own rule in shake.
_EXCEPTIONS :: [String]
_EXCEPTIONS = ["server"]


-- * TODO
--
-- currently nothing


-- * EXTRA COMMAND LINE ARGUMENTS

data BuildOptions = BuildOptions
    { jobs :: Int
    , oLevel :: Int
    , prof :: Bool
    , cabalgenOptions :: String
    , ghcOptions :: String
    , cabalBuildOptions :: String
    , isCIRun :: Bool
    , extsrcBuildingStrategy :: ExtsrcBuildingStrategy
    } deriving (Eq, Show)

defaultBuildOptions :: BuildOptions
defaultBuildOptions = BuildOptions
    { jobs = -1 -- set by `nproc`
    , oLevel = 2
    , prof = False
    , cabalgenOptions = ""
    , ghcOptions = ""
    , cabalBuildOptions = ""
    , isCIRun = False
    , extsrcBuildingStrategy = ExtsrcAll
    }

optDescrs :: [OptDescr (Either String (BuildOptions -> BuildOptions))]
optDescrs = map colorOptionHack
    [ Option "O" []

        (optional "N" (\o n -> o{ oLevel = n }) naturalIntArg)
        "optimization level to pass to GHC via -O"

    , Option "" ["prof"]

        (NoArg $ Right (\o -> o{ prof = True }))
        "enable profiling"

    , Option "" ["cabalgen-options"]

        (optional "OPTS" (\o str -> o{ cabalgenOptions = str }) Right)
        "options to pass to ./cabalgen.py"

    , Option "" ["cabal-build-options"]

        (optional "OPTS" (\o str -> o{ cabalBuildOptions = str }) Right)
        "options to pass to cabal build"

    , Option "" ["ghc-options"]

        (optional "OPTS" (\o str -> o{ ghcOptions = str }) Right)
        "options to pass to ghc"

    , Option "" ["extsrc-dev"]

        (NoArg $ Right (\o -> o{ extsrcBuildingStrategy = ExtsrcOnlyChanged }))
        "build only those extsrc packages that have changes. Faster, but you will get a package hash error if other extsrc depend on your change"

    , Option "" ["ci"]

        (NoArg $ Right (\o -> o{ isCIRun = True, extsrcBuildingStrategy = ExtsrcAll }))
        "whether this is a Continuous Integration"
    ]
    where
        -- Colorizes option descriptions to tell Shakes and our options apart.
        colorOptionHack (Option short long argDescr descr) =
            Option short long argDescr (red descr)

-- | Helper to create an optional argument.
--
-- > optional "PLACEHOLDER" (\o parsedValue -> o{ ... = parsedValue }) parsing-function-or-simply-Right
optional :: String                              -- ^ a placeholder to show (e.g. --add FILES)
         -> (BuildOptions -> a -> BuildOptions) -- ^ record update of the `BuildOptions`, given the parsed value
         -> (String -> Either String a)         -- ^ how to parse the value from a string
         -> ArgDescr (Either String (BuildOptions -> BuildOptions))
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

-- | The name (no path) of this file we are in.
_THIS_FILE_NAME :: String
_THIS_FILE_NAME = takeFileName $(LitE . StringL . loc_filename <$> location)

-- | Gets the absolute root directory of this git repository. Errors on failure.
getGitRoot :: IO FilePath
getGitRoot = do
    (exitCode, projectRootStr, stderr) <- readProcessWithExitCode "git" ["rev-parse", "--show-toplevel"] ""
    case exitCode of
        ExitSuccess   -> return $ strip projectRootStr
        ExitFailure n -> error $ "getGitRoot exit code " ++ show n ++ ":\n" ++ stderr

main :: IO ()
main = do
    -- cd "hack" so that this file can be called from anywhere.
    projectRoot <- getGitRoot
    IO.setCurrentDirectory (projectRoot </> _BUILD_TOP_LEVEL_DIR)

    -- Get a checksum of THIS FILE to use as shake version.
    -- This will trigger a rebuild when these rules are changed.
    -- Sometimes you want to comment it out when you are developing this file
    -- and when you know that your rules do not get out of sync with .shake.database.
    -- This can be turned off by touching the file DEVELOPING_SHAKE_DO_NOT_CHECK_IN.
    hashedShakeVersion <- do
        skipChecksum <- IO.doesFileExist "DEVELOPING_SHAKE_DO_NOT_CHECK_IN"
        if skipChecksum
            then return "DEVELOPING_SHAKE_DO_NOT_CHECK_IN"
            else do checksum <- dropWhile (== '-') . show . hashWithSalt 0 <$> BS.readFile _THIS_FILE_NAME
                    return $ "hash-" ++ checksum ++ "-" ++ _THIS_FILE_NAME

    defaultJobs <- getCores
    j           <- hackShakeJobs defaultJobs

    -- Get all targets listed in cabalgen.
    availableCabalTargets <- getCabalgenExecutables

    -- NOTE: Shake (still as of 0.10.6) will cut off the version change
    --       message after `.` symbols (only in its display logic).
    --       See https://github.com/ndmitchell/shake/issues/39.
    let shakeOpts = shakeOptions
            { shakeLint = True
            , shakeVersion = hashedShakeVersion
            -- , shakeReport = Just "_make/build-report.html"
            , shakeThreads = j
            }

    ((), took) <- time . hackColors $ shakeArgsWithRecord shakeOpts optDescrs defaultBuildOptions $
        \buildOpts cliTargets -> do

            -- Cabal targets get mapped to their final location in dist/build/
            -- so that shake can check if they were created successfully.
            let resolvedGroups = concatMap exeGroup cliTargets
                cabalTargets     = resolvedGroups `intersect` availableCabalTargets
                normalTargets    = resolvedGroups `without`   availableCabalTargets
                exceptionTargets = resolvedGroups `intersect` _EXCEPTIONS
                targets = normalTargets ++ map toExeTarget cabalTargets ++ exceptionTargets

            return $ Just $ rules targets
                                  availableCabalTargets
                                  buildOpts{ jobs = j }

    -- Notify of done build if it took longer than 10s.
    when (took > 10.0) $ notifyIO "done"

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
                    <$> readProcessOrDie "nproc" [] ""

        -- Cheat in colors until https://github.com/ndmitchell/shake/issues/49
        -- if $COLORTERM is set and not empty
        hackColors f = do args <- getArgs
                          col <- lookupEnv "COLORTERM"
                          case col of
                              Just x | x /= "" -> withArgs ("--color":args) f
                              _                -> withArgs args f

        -- For now does not contain prefixed equivalents like "lib:..." and "exe:...".
        getCabalgenExecutables :: IO [String]
        getCabalgenExecutables = words <$> readProcessOrDie "./cabalgen.py" ["-dump-executables"] ""



-- * CUSTOM OPTIONS DATA TYPES

data ExtsrcBuildingStrategy = ExtsrcAll         -- ^ run cabal build on all extsrc packages together
                            | ExtsrcOnlyChanged -- ^ run cabal build only in those extsrc packages
                                                --   that have changed source files (faster, but
                                                --   but you will get a package hash error if other
                                                --   extsrc depend on your change)
                            deriving (Eq, Ord, Show)



-- * THE BUILD RULES

-- READ THIS for adding new rules:
--
-- * "Phony actions are intended to define command-line abbreviations.
--    You should not need phony actions as dependencies of rules [...]."
--   Means: Never `need ["myphony"]`, always `need ["file-needed-in-phony"]`;
--   best make a let binding for it, e.g. in the "Common stamp files" section.
--
-- * Shake directory scans and especially FilePattern matches are slow
--   (https://github.com/ndmitchell/shake/issues/47). If you have simple
--   patterns like `//*` or all files in a directory, potentially filtered,
--   use posix-paths based alternatives like `allFilesIn` - its 30 times faster.
rules :: [String] -> [String] -> BuildOptions -> Rules ()
rules targets availableCabalTargets buildOptions@BuildOptions{ jobs, oLevel, prof, cabalgenOptions, ghcOptions, cabalBuildOptions, extsrcBuildingStrategy, isCIRun } = do

    -- Common stamp files
    let ghcVersionFile      = "_make/ghc/version"
        ghcPkgList          = "_make/cabal-dev/pkglist"
        extsrc              = "_make/extsrc/packages-install-stamp"
        extsrcLeftoverCheck = "_make/extsrc/leftover-check-stamp"
        cabalConfig         = "_make/project/setup-config"
        libStamp            = "_make/project/library-build-stamp" -- stamp for only building the library
        typecheckStamp      = "_make/project/typecheck-stamp"
        commitStamp         = "_make/git-commit-sha"
        versionFile         = "src/Version.hs"

    -- Phonys - never `need` these; instead need their files
    phony "all"         $ need ["build-all"]
    phony "extsrc"      $ need [extsrc]
    phony "cabalgen"    $ need [_CABAL_FILE]
    phony "configure"   $ need [cabalConfig]
    phony "lib"         $ need [libStamp]
    phony "tags"        $ need _TAGS_FILES
    phony "hscope"      $ need ["hscope.out"]
    phony "typecheck"   $ need [typecheckStamp]


    phony "help" $ do
        liftIO . putStrLn . unlines $
            [ "BUILDING"
            , ""
            , "  Build.hs                   Compile all code (fastest safe one)"
            , "  Build.hs --help            Help for options that are not build targets, e.g.:"
            , "                                -O"
            , "                                --prof"
            , "                                --ghc-options"
            , "                                --cabal-build-options"
            , "                                --trace"
            , "                                -q --quiet"
            , "  Build.hs all               Compile all code + build all executables"
            , "  Build.hs typecheck         cabal-dev build only; does not notice extsrc changes (fastest)"
            , "  Build.hs [xxx]             Build only executable or group named [xxx]"
            , "  Build.hs install           Symlink to all executables in dist/ from ../../release/bin"
            , "  Build.hs cleanbuild        Clean everything, build optimized, install"
            , "  Build.hs tags              Generate tags file"
            , "  Build.hs extsrc            Just install extsrc in the sandbox"
            , ""
            , "TESTING"
            , ""
            , "  Build.hs test              Compile all code + run tests"
            , ""
            , "BENCHMARKING"
            , ""
            , "  Build.hs bench             Compile all code + run benchmarks"
            , ""
            , "PROFILING"
            , ""
            , "  Build.hs --prof [targets]"
            , ""
            , "  * when building with profiling enabled, -auto is added by default."
            , "  * other options can be provided as usual by e.g. --ghc-options=\"-fprof-auto\"."
            , ""
            , "CLEANING"
            , ""
            , "  Build.hs clean             Wipe this project"
            , "  Build.hs clean-extsrc      Wipe extsrc"
            , "  Build.hs clean-all         Both above + wipe all Shake stamp files"
            , ""
            , "CHANGING THE BUILD"
            , ""
            , "  * Modules are crawled from src/ automatically."
            , "  * To add a new executable, change cabalgen.py."
            , "  * To add a new group, change `exeGroup` in this file."
            , "  * To change some other cabal option (e.g. version bounds),"
            , "    change myproject.cabal.template."
            , "  * Do not forget that Shake has a lot of useful features; see Build.hs --help."
            ]


    -- Default target
    action $ do
        putNormal $ "Build options: " ++ show buildOptions

        putLoud $ "targets: " ++ unwords targets

        -- Run actions that need to run first, e.g. such that clean the build.
        cleanOnOptimizationChange oLevel isCIRun -- always check whether -O flags changed
        need [versionFile]                       -- always touch version file on commit change

        -- If no targets are given, run the ones we use most.
        -- TODO change this to the ones we ran last time?
        if targets /= []
            then need targets
            else need $ ["lib"] ++ _TAGS_FILES


    -- * ENVIRONMENT SECTION

    -- GHC version
    ghcVersionFile *> \out -> do
        alwaysRerun
        Stdout v <- cmd "ghc --numeric-version"
        writeFileChanged out v

    -- Current git commit SHA
    commitStamp *> \out -> do
        alwaysRerun
        Stdout sha <- cmd "git rev-parse HEAD"
        writeFileChanged out sha

    -- * CABAL SECTION


    _CABAL_FILE *> \_ -> do
        alwaysRerun -- cabalgen only takes 130 ms
        let profFlag = if prof then "-prof" else "" -- cabalgen generates a different cabal file on -prof
        Stdout _ <- cmd "./cabalgen.py" cabalgenOptions profFlag -- silence
        return ()


    -- Available packages (ghc-pkg list inside cabal-dev)
    ghcPkgList *> \out -> do
        alwaysRerun
        Stdout src <- cmd "cabal-dev ghc-pkg list -v"
        writeFileChanged out src


    -- cabal configure


    -- Runs cabal-dev configure to obtain the setup-config, fixing all dependency versions.
    cabalConfig *> \_ -> do
        -- We need extsrc ready for this so cabal configure can check the dependency versions.
        need [_CABAL_FILE, extsrc]
        -- Depend on GHC version and all package hashes.
        -- Note that this breaks if you try to cheat packages in around cabal!
        need [ghcVersionFile, ghcPkgList]

        let profFlag = if prof
                         then "--enable-library-profiling --enable-executable-profiling"
                         else "--disable-library-profiling --disable-executable-profiling"

        Stdout _ <- cmd "cabal-dev configure" profFlag -- silence

        -- cabal configure will always touch the setup-config.
        -- Copy if it to our own file if it has not changed such that it gets
        -- touched only when the contents actually changed.
        copyFileIfChanged "dist/setup-config" cabalConfig


    -- Version file
    versionFile *> \_ -> do
        need [commitStamp]
        cmd "touch src/Version.hs"


    -- cabal build

    -- Runs cabal-dev build.
    let cabalBuild :: [String] -> Action ()
        cabalBuild cabalNamedTargets = do
            -- We track extsrc, the cabal config and all source files used by cabal
            need [extsrc, cabalConfig, versionFile]
            need =<< liftIO _CABAL_BUILD_SOURCE_FILES

            let ghcOpts = unwords [ "-j " ++ show jobs            -- jobs, with space for parmake
                                  , "-O" ++ show oLevel           -- optimization
                                  , if prof then "-auto" else ""  -- -auto for profiling
                                  , ghcOptions
                                  ]

            v <- getVerbosity

            -- Run the build
            cmd (EchoStdout (v >= Normal))
                "cabal-dev build"
                "--with-ghc=ghc-parmake"["--ghc-options=" ++ ghcOpts]
                ("-j" ++ show jobs) -- jobs, for cabal itself
                cabalBuildOptions
                (unwords cabalNamedTargets)


    -- Library only target
    libStamp *> \stamp -> do
        cabalBuild ["lib:" ++ _CABAL_NAME]
        writeFile' stamp ""

    -- Compiles cabal targes specified on the command line with *one* cabal invocation.
    --
    -- We use:
    --     cabalExes             *>>   cabalNames
    -- like:
    --     dist/build/prog/prog        cabal build prog
    let cabalExes = map toExeTarget availableCabalTargets -- all possible dist/build/*/*
    cabalExes *>> \_ -> do
        let cabalNames = [ takeFileName t | t <- targets, "dist/build/" `isPrefixOf` t ]
        cabalBuild cabalNames

    -- all target
    phony "build-all" $ do
        cabalBuild [] -- cabal build without more arguments builds everything
        need ["server"] -- also build our web projects


    -- * EXTSRC SECTION
    --
    -- extsrc (external packages, available as source in the file system)

    -- Inspired by https://github.com/ndmitchell/shake/issues/38

    -- Removes extsrc remainders that can mess up our build (source + registered packages)
    extsrcLeftoverCheck *> \stamp -> do
        alwaysRerun -- there is no situation we may skip this rule

        -- Kill all folders that have no .cabal file
        putNormal "Checking for old packages in extsrc"
        liftIO $ removeNonCabalPackages isCIRun

        -- Unregister all packages that have no equivalent in extsrc
        putNormal "Checking for old registered packages in the package database"
        unregisterNonExtsrcPackages =<< (strip <$> readFile' ghcVersionFile) -- TODO use oracle for this

        writeFile' stamp ""

    -- Installs all cabal packages in _PACKAGES_DIR that have been changed.
    -- Time stamp file gets touched if something was installed.
    extsrc *> \stamp -> do
        alwaysRerun -- we always want to run this rule since the user could e.g.
                    -- have added packages from extsrc

        -- Purge extsrc leftovers
        need [extsrcLeftoverCheck]

        -- Discover packages
        allPackages <- sort <$> getDirectoryDirs _PACKAGES_DIR -- TODO use set for this

        -- Make sure all extsrc packages are installed
        ghcVersion <- strip <$> readFile' ghcVersionFile
        registeredPkgs <- cabalDevGhcPkgList ghcVersion
        let missingPkgs = allPackages `without` registeredPkgs
        when (registeredPkgs /= allPackages) $
            alwaysRerun

        -- Which packages have source code changes?
        changedPackages <- flip filterM allPackages $ \p -> do
            need ["_make/extsrc" </> p <.> "stamp"]
            -- Use the non-shake version here to not require the .rebuild to
            -- exist in shake's lint (we create and delete it in one rule run).
            liftIO . IO.doesFileExist $ "_make/extsrc" </> p <.> "rebuild"

        -- Which packages should be rebuilt?
        let toInstall = case extsrcBuildingStrategy of
                ExtsrcOnlyChanged                 -> sort (changedPackages ++ missingPkgs)
                ExtsrcAll | changedPackages /= [] -> allPackages -- if some package changed, install all of them
                ExtsrcAll                         -> missingPkgs
        -- Installing only the package(s) with changed files might break safely.
        -- Warn for that.
        when (toInstall `notElem` [[], allPackages]) $
            putQuiet . red $ "This is a selective extsrc install. " ++
                             "You will get a package hash error if other extsrc " ++
                             "depend on your change."

        when (toInstall /= []) $ do
            -- Build/install packages
            putNormal . unlines $ ["Building with cabal:"] ++ map ("  - " ++) toInstall
            cabalDevInstall jobs [ _PACKAGES_DIR </> p | p <- toInstall ]

            -- Mark packages as installed
            forM_ changedPackages $ \p ->
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


    -- * SPECIAL TARGETS SECTION
    --
    -- If a phony you wish to define here coincides with the name of a cabal
    -- executable, you have to add it to the `_EXCEPTIONS`.
    -- You should `need` it's executable in dist/build/*/* here for
    -- clarity; however, the `cabalExes` target will build it anyway.

    typecheckStamp *> \_ -> do
        -- Does a very fast O0 build; runs in a separate output folder such
        -- that it doesn't interfere with usual -prof or -O2 builds.
        need [extsrc, cabalConfig, versionFile]
        need =<< liftIO _CABAL_BUILD_SOURCE_FILES
        -- We don't use parmake here since it has an overhead when little has changed and we're in -O0
        () <- cmd "cabal build"
                  ["--ghc-options=-O0 -hidir dist/o0 -odir dist/o0 " ++ ghcOptions]
                  ("lib:" ++ _CABAL_NAME)
        writeFile' typecheckStamp ""

    phony "sreport" $ do
        need [toExeTarget "sreport-web"]
        -- Need bash for `source`
        cmd (Cwd "src/Tools/SReport/web") "bash -c" ["source setup.source.sh && make"]

    phony "server" $ do
        need [toExeTarget "server"]
        cmd "make -C ../../web"

    phony "install" $ do
        need ["all"] -- need to build all executables in order for find to find them

        -- Create release/bin dir
        () <- cmd "mkdir -p ../../release/bin"

        -- Symlink all executables
        () <- cmd Shell ["find bin cabal-bin dist/build -executable -type f -exec ln -f -s ../../src/myproject/{} ../../release/bin \\;"]
        return ()

    phony "test" $ do
        -- We need to install everything since the tests can call executables.
        need ["all", "install"]

        putNormal "Running unit tests"
        () <- cmd "dist/build/tests-unit/tests-unit"

        putNormal "Running system tests"
        cmd $ "dist/build/tests-system/tests-system -j" ++ show jobs

    phony "bench" $ do
        need $ map toExeTarget ["bench-exe"]
        need ["install"]

        putNormal "Running bench-inst"
        () <- cmd "dist/build/bench-exe/bench-exe"
        return ()

    phony "fullclean" $ do
        liftIO . putStrLn . red $ "WARNING: fullclean target is deprecated, use cleanbuild"
        need ["cleanbuild"]

    phony "cleanbuild" $ do
        need ["clean-all"]
        need ["all"]


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

        () <- cmd "make -C ../../web clean"

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


    -- * CODE TOOLS SECTION

    -- Tags (hasktags)
    _TAGS_FILES *>> \_ -> do
        hsFiles <- filter ((== ".hs") . takeExtension) <$> liftIO (allFilesIn [".", _PACKAGES_DIR])
        need hsFiles
        cmd (Cwd _TAGS_ROOT) $ "hasktags --extendedctag " ++ _TAGS_SCAN_DIRS

    -- Cross references (hscope)
    "hscope.out" *> \_ -> do
        hsFiles <- filter ((== ".hs") . takeExtension) <$> liftIO (allFilesIn [".", _PACKAGES_DIR])
        need hsFiles
        cmd $ "hscope --build " ++ unwords hsFiles



-- * HELPERS

-- | Checks whether the -O optimization level has changed, and if yes, calls clean.
-- This must be done BEFORE any other rules are run!
cleanOnOptimizationChange :: Int -> Bool -> Action ()
cleanOnOptimizationChange newOLevel isCIRun = do

    let lastOFile = "_make/last-O" -- stores the last used -O level

    -- Cannot use shake's tracking file operations here since
    -- we read from and change the file in one run.

    exists <- liftIO $ IO.doesFileExist lastOFile
    when exists $ do
        lastOLevel <- fromEither (lastOFile ++ ": ") . naturalIntArg
                      <$> liftIO (readFile lastOFile)

        -- When the optimization level changes, we need to rebuild.
        -- We don't need to rebuild extsrc because we always build that with -O2.
        when (newOLevel /= lastOLevel) $ do
            putQuiet . red $ "Optimization level changed from " ++ show lastOLevel ++ " to " ++ show newOLevel

            unless isCIRun $ do
                notify "CONFIRM: O level changed"
                liftIO . putStrLn $ "Press Enter to clean the build, or Ctrl-C to abort"
                void $ liftIO getLine

            need ["clean"]

    -- lastOLevel file is not managed by shake, so we have to make parent
    -- directories ourselves.
    liftIO $ createDirectoryIfMissing True (takeDirectory lastOFile)
    writeFileChanged lastOFile (show newOLevel ++ "\n")


-- | Uses `cabal-dev install` to install the packages in the given directories.
-- The directories MUST contain `.cabal` files!
cabalDevInstall :: Int -> [String] -> Action ()
cabalDevInstall _ [] = return () -- nothing to do if no paths given
cabalDevInstall nJobs packagePaths = do
    v <- getVerbosity
    cmd (EchoStdout (v >= Normal)) -- We always compile with profiling for external packages.
        "cabal-dev install --sandbox=cabal-dev --force-reinstalls"
        "--enable-library-profiling --disable-documentation"
        ("-j" ++ show nJobs)
        -- Append trailing slashes to make sure cabal treats it as
        -- directories instead of package names
        (map addTrailingPathSeparator packagePaths)


-- | Runs ghc-pkg list --simple-output in the cabal-dev/ sandbox.
-- Returns the available packages sorted.
-- If some packages have () or {} around them (hidden, broken),
-- this function will return that literally.
-- TODO Figure out what to do with broken/hidden packages.
cabalDevGhcPkgList :: String -> Action [String]
cabalDevGhcPkgList ghcVersion = do
    let packageDB = "cabal-dev/packages-" ++ ghcVersion ++ ".conf"
    exists <- liftIO . IO.doesDirectoryExist $ packageDB
    if not exists
        then return []
        else do Stdout out <- cmd "ghc-pkg --no-user-package-db"
                                  "--package-db" packageDB
                                  "list --simple-output"
                return $ sort (words out)


-- | Deletes child directories that have no .cabal file in them.
-- Errors unless we are in Continuous Integration (to not delete user files).
removeNonCabalPackages :: Bool -> IO ()
removeNonCabalPackages isCIRun = do
    -- TODO Get IO based shake function for this (https://github.com/ndmitchell/shake/issues/58)
    packages <- getExtsrcDirs

    forM_ [ _PACKAGES_DIR </> p | p <- packages ] $ \dir -> do
        files <- IO.getDirectoryContents dir
        let noCabalFile = null [ f | f <- files, takeExtension f == ".cabal" ]
        when noCabalFile $ do
            if isCIRun
                then do putStrLn $ "We are in CI, so mercilessly removing " ++ dir
                        IO.removeDirectoryRecursive dir
                else error $ "extsrc dir " ++ dir ++ " has no cabal file - you should delete it"


-- | Removes all packages from the sandbox that are not in extsrc.
unregisterNonExtsrcPackages :: String -> Action ()
unregisterNonExtsrcPackages ghcVersion = do
    registered <- cabalDevGhcPkgList ghcVersion
    extsrcPkgs <- liftIO getExtsrcDirs

    let toRemove = registered `without` extsrcPkgs
        packageDB = "cabal-dev/packages-" ++ ghcVersion ++ ".conf"

    when (toRemove /= []) $ do
        putQuiet . red $ "Unregistering sandbox packages that are not in extsrc: "
                         ++ unwords toRemove
        forM_ toRemove $ \p -> do
            () <- cmd "ghc-pkg --no-user-package-db"
                       "--package-db" packageDB
                       "unregister --force" p -- force will break packages, but we don't care,
                                              -- since we will install everything that's required
            return ()


-- | Print something in red.
red :: String -> String
red msg = escape "31" msg
    where
        escape code x = "\ESC[" ++ code ++ "m" ++ x ++ "\ESC[0m"

-- | Strips whitespace on both sides.
strip :: String -> String
strip = T.unpack . T.strip . T.pack


-- | Displays a visual notification using `notify-send`.
-- Does nothing if `notify-send` is not available.
notify :: String -> Action ()
notify msg = do
    Exit _ <- cmd Shell ("notify-send \"Build -> " ++ msg ++ "\"")
    return ()

notifyIO :: String -> IO ()
notifyIO msg = do
    _ <- system ("notify-send \"Build -> " ++ msg ++ "\" 2>/dev/null")
    return ()


-- | Times an IO action, returns how long it took in seconds.
time :: IO a -> IO (a, Double)
time act = do
    before <- getCurrentTime
    res <- act
    after <- getCurrentTime
    return (res, realToFrac $ after `diffUTCTime` before)


-- TODO Ask for shake inclusion
getDirectoryFilesPrefixed :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFilesPrefixed dir pattern = map (dir </>) <$> getDirectoryFiles dir pattern


-- TODO Ask for shake inclusion
copyFileIfChanged :: FilePath -> FilePath -> Action ()
copyFileIfChanged src dest = liftIO (readFile src) >>= writeFileChanged dest


-- | Runs the action if the file/directory exists.
-- Does not use Shake's file tracking.
whenExists :: (MonadIO m) => FilePath -> m () -> m ()
whenExists path act = do
    file <- liftIO $ IO.doesFileExist path
    dir  <- liftIO $ IO.doesDirectoryExist path
    when (dir || file) act


-- | Gets all files (no directories) in any of the given directories.
-- Uses posix-paths, so it's much faster than Shake's "//*" pattern.
allFilesIn :: [FilePath] -> IO [FilePath]
allFilesIn dirs = concat <$> mapM findFiles dirs


-- | Lists all files (no directories) in a given root + subdirectories.
findFiles :: FilePath -> IO [FilePath]
findFiles root = map BS8.unpack <$> allDirectoryFiles' (BS8.pack root)


-- | Lists all files that are not directories.
allDirectoryFiles' :: RawFilePath -> IO [RawFilePath]
allDirectoryFiles' root = allDirectoryContents root
                          >>= filterM (\fp -> not . isDirectory <$> getFileStatus fp)
-- TODO: The following is better but relies on posix-paths >= 0.1.1.0,
--       allDirectoryContents'/traverseDirectory were broken before it
--       (https://github.com/JohnLato/posix-paths/pull/5):
-- allDirectoryFiles' root = reverse <$> traverseDirectory collectList [] root
--     where
--         collectList acc fp = do isDir <- not . isDirectory <$> getFileStatus fp
--                                 return $ if isDir then acc else (fp:acc)


-- | All top-level directories in _PACKAGES_DIR.
getExtsrcDirs :: IO [String]
getExtsrcDirs = filter (`notElem` [".", ".."])
                <$> IO.getDirectoryContents _PACKAGES_DIR


-- | Gets all files of a cabal project in _PACKAGES_DIR, excluding dist/ files.
-- Example:  getCabalPackageFiles "webserver-0.7.1.0"
getCabalPackageFiles :: String -> IO [FilePath]
getCabalPackageFiles p = do
    let packageRoot = _PACKAGES_DIR </> p
        relative path = makeRelative packageRoot path -- path of file from package root

    -- Use non-Shake file list function to not track the files we want to exclude.
    packageFiles <- liftIO $ findFiles packageRoot

    return [ f | f <- packageFiles, takeDirectory1 (relative f) /= "dist" ]


-- | Runs the given program with arguments and stdin.
-- Terminates the program if running the program failed with an exit code,
-- printing an error message and the program's stderr.
readProcessOrDie :: String -> [String] -> String -> IO String
readProcessOrDie program args input = do
    (exitCode, out, err) <- readProcessWithExitCode program args input
    case exitCode of
        ExitSuccess   -> return out
        ExitFailure n -> do putStrLn . red $ unlines
                                [ "Program failed with exit code " ++ show n ++ ":"
                                , unwords (program : args)
                                , ""
                                , "STDOUT:"
                                , ""
                                , err
                                ]
                            exitWith exitCode


-- | Set-based list intersection.
intersect :: (Ord a) => [a] -> [a] -> [a]
intersect xs ys = Set.toList $ Set.fromList xs `Set.intersection` Set.fromList ys

-- | Set-based list difference (xs \\ ys).
without :: (Ord a) => [a] -> [a] -> [a]
without xs ys = Set.toList $ Set.fromList xs `Set.difference` Set.fromList ys


-- | Where an executable should end up after being built with cabal.
toExeTarget :: String -> FilePath
toExeTarget t = "dist/build" </> t </> t
