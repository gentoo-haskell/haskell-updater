{- |
   Module      : Main
   Description : The haskell-updater executable
   Copyright   : (c) Ivan Lazar Miljenovic, Stephan Friedrichs, Emil Karlson 2010
   License     : GPL-2 or later

   The executable module of haskell-updater, which finds Haskell
   packages to rebuild after a dep upgrade or a GHC upgrade.
-}
module Main (main) where

import Distribution.Gentoo.GHC
import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager

import           Control.Monad         (unless)
import           Data.Char             (toLower)
import           Data.Either           (partitionEithers)
import           Data.List             (foldl1')
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import qualified Data.Set              as Set
import           Data.Version          (showVersion)
import qualified Paths_haskell_updater as Paths (version)
import           System.Console.GetOpt
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (ExitCode (..), exitSuccess, exitWith)
import           System.IO             (hPutStrLn, stderr)
import           System.Process        (rawSystem)

import Output

-- -----------------------------------------------------------------------------
-- The overall program.

main :: IO ()
main = do args <- getArgs
          defPM <- defaultPM
          case parseArgs defPM args of
              Left err -> die err
              Right a  -> uncurry runAction a
-- -----------------------------------------------------------------------------
-- The possible actions that haskell-updater can perform.

data Action = Help
            | Version
            | Build { target :: BuildTarget }
              -- If anything is added here after Build, MAKE SURE YOU
              -- UPDATE combineActions or it won't always work!
              deriving (Eq, Ord, Show, Read)

defaultAction :: Action
defaultAction = Build OnlyInvalid

-- Combine all the actions together.  If the list is empty, use the
-- defaultAction.
combineAllActions :: [Action] -> Action
combineAllActions = emptyElse defaultAction (foldl1' combineActions)

-- Combine two actions together.  If they're both Build blah, merge
-- them; otherwise, pick the lower of the two (i.e. more important).
-- Note that it's safe (at the moment at least) to assume that when
-- the lower of one is a Build that they're both build.
combineActions       :: Action -> Action -> Action
combineActions a1 a2 = case a1 `min` a2 of
                         Help    -> Help
                         Version -> Version
                         Build{} -> Build $ target a2 -- later options has priority

runAction :: RunModifier -> Action -> IO a
runAction rm action =
    case action of
        Help        -> help
        Version     -> version
        Build t     -> do systemInfo v rm t
                          ps <- getTargetPackages v t
                          if listOnly rm
                              then mapM_ (putStrLn . printPkg) ps
                              else buildPkgs rm ps
                          success v "done!"
  where v = verbosity rm
-- -----------------------------------------------------------------------------
-- The possible things to build.

data BuildTarget = OnlyInvalid
                 | AllInstalled -- Rebuild every haskell package
                   deriving (Eq, Ord, Show, Read)

getTargetPackages :: Verbosity -> BuildTarget -> IO [Package]
getTargetPackages v t =
    case t of
        OnlyInvalid -> do say v "Searching for packages installed with a different version of GHC."
                          say v ""
                          old <- oldGhcPkgs v
                          pkgListPrintLn v "old" old

                          say v "Searching for Haskell libraries with broken dependencies."
                          say v ""
                          (broken, unknown_packages, unknown_files) <- brokenPkgs v
                          printUnknownPackagesLn (map unCPV unknown_packages)
                          printUnknownFilesLn unknown_files
                          pkgListPrintLn v "broken" (notGHC broken)

                          return $ Set.toList $ Set.fromList $ old ++ broken

        AllInstalled -> do say v "Searching for packages installed with the current version of GHC."
                           say v ""
                           pkgs <- allInstalledPackages
                           pkgListPrintLn v "installed" pkgs
                           return pkgs

  where printUnknownPackagesLn [] = return ()
        printUnknownPackagesLn ps =
            do say v "The following packages are orphan (not installed by your package manager):"
               printList v id ps
               say v ""
        printUnknownFilesLn [] = return ()
        printUnknownFilesLn fs =
            do say v $ "The following files are orphan (not installed by your package manager):"
               printList v id fs
               say v $ "It is strongly advised to remove orphans:"
               say v $ "    One of known sources of orphans is packages installed before 01 Jan 2015."
               say v $ "    If you know it's your case you can easily remove such files:"
               say v $ "        # rm -v -- `qfile -o $(ghc --print-libdir)/package.conf.d/*.conf $(ghc --print-libdir)/gentoo/*.conf`"
               say v $ "        # ghc-pkg recache"
               say v $ "    It will likely need one more 'haskell-updater' run."
               say v ""

-- -----------------------------------------------------------------------------
-- How to build packages.

data RunModifier = RM { pkgmgr   :: PkgManager
                      , flags    :: [PMFlag]
                      , withCmd  :: WithCmd
                      , rawPMArgs :: [String]
                      , verbosity :: Verbosity
                      , listOnly :: Bool
                      }
                   deriving (Eq, Ord, Show, Read)

-- At the moment, PrintAndRun is the only option available.
data WithCmd = RunOnly
             | PrintOnly
             | PrintAndRun
               deriving (Eq, Ord, Show, Read)

type WithUserCmd = Either String WithCmd

withCmdMap :: Map String WithCmd
withCmdMap = M.fromList [ ("print", PrintOnly)
                        , ("run", RunOnly)
                        , ("print-and-run", PrintAndRun)
                        ]

defaultWithCmd :: String
defaultWithCmd = "print-and-run"

runCmd :: WithCmd -> String -> [String] -> IO a
runCmd mode cmd args = case mode of
        RunOnly     ->                      runCommand cmd args
        PrintOnly   -> putStrLn cmd_line >> exitSuccess
        PrintAndRun -> putStrLn cmd_line >> runCommand cmd args
    where cmd_line = unwords (cmd:args)

runCommand     :: String -> [String] -> IO a
runCommand cmd args = rawSystem cmd args >>= exitWith

buildPkgs       :: RunModifier -> [Package] -> IO a
buildPkgs rm  [] = success (verbosity rm) "\nNothing to build!"
buildPkgs rm ps = runCmd (withCmd rm) cmd args
    where
      (cmd, args) = buildCmd (pkgmgr rm) (flags rm) (rawPMArgs rm) ps

-- -----------------------------------------------------------------------------
-- Command-line flags

data Flag = HelpFlag
          | VersionFlag
          | PM String
          | CustomPMFlag String
          | FixInvalid
          | RebuildAll
          | Pretend
          | NoDeep
          | QuietFlag
          | VerboseFlag
          | ListOnlyFlag
          | Cmd String
          deriving (Eq, Ord, Show, Read)

parseArgs :: PkgManager -> [String] -> Either String (RunModifier, Action)
parseArgs defPM args = argParser defPM $ getOpt' Permute options args

argParser :: PkgManager
          -> ([Flag], [String], [String], [String])
          -> Either String (RunModifier, Action)
argParser dPM (fls, nonoptions, unrecognized, errs)
    | (not . null) errs         = Left $ unwords $ "Errors in arguments:" : errs
    | (not . null) unrecognized = Left $ unwords $ "Unknown options:" : unrecognized
    | (not . null) bPms         = Left $ unwords $ "Unknown package managers:" : bPms
    | (not . null) bCmds        = Left $ unwords $ "Unknown action:" : bCmds
    | otherwise                 = Right (rm, a)
  where
      (fls', as) = partitionBy flagToAction fls
      a = combineAllActions as
      (fls'', pms) = partitionBy flagToPM fls'
      (bPms, pms') = partitionBy isValidPM pms
      (opts, cmds') = partitionBy flagToCmd fls''
      (bCmds, cmds) = partitionBy isValidCmd cmds'
      pm = emptyElse dPM last pms'
      opts' = Set.fromList opts
      cmd = emptyElse (fromJust $ M.lookup defaultWithCmd withCmdMap) last cmds
      hasFlag = flip Set.member opts'
      pmFlags = bool id (PretendBuild:) (hasFlag Pretend)
                . return $ bool UpdateDeep UpdateAsNeeded (hasFlag NoDeep)
      rm = RM { pkgmgr   = pm
              , flags    = pmFlags
              , withCmd  = cmd
              , rawPMArgs = nonoptions
              , verbosity = case () of
                                _ | hasFlag VerboseFlag -> Verbose
                                _ | hasFlag QuietFlag   -> Quiet
                                _                       -> Normal
              , listOnly  = hasFlag ListOnlyFlag
              }

flagToAction             :: Flag -> Either Flag Action
flagToAction HelpFlag    = Right Help
flagToAction VersionFlag = Right Version
flagToAction FixInvalid  = Right . Build $ OnlyInvalid
flagToAction RebuildAll  = Right . Build $ AllInstalled
flagToAction f           = Left f

flagToPM                   :: Flag -> Either Flag PkgManager
flagToPM (CustomPMFlag pm) = Right $ stringToCustomPM pm
flagToPM (PM pm)           = Right $ choosePM pm
flagToPM f                 = Left f

flagToCmd :: Flag -> Either Flag WithUserCmd
flagToCmd (Cmd cmd) = Right $ chooseCmd cmd
flagToCmd f = Left f

chooseCmd :: String -> WithUserCmd
chooseCmd cmd = chooseCmd' $ map toLower cmd
  where
    chooseCmd' :: String -> WithUserCmd
    chooseCmd' "run"   = Right RunOnly
    chooseCmd' "print" = Right PrintOnly
    chooseCmd' "print-and-run" = Right PrintAndRun
    chooseCmd' c = Left c

isValidCmd :: WithUserCmd -> Either String WithCmd
isValidCmd = id

options :: [OptDescr Flag]
options =
    [ Option ['c']      ["dep-check"]       (NoArg FixInvalid)
      "Check dependencies of Haskell packages."
    -- deprecated alias for 'dep-check'
    , Option ['u']      ["upgrade"]         (NoArg FixInvalid)
      "Rebuild Haskell packages after a GHC upgrade."
    , Option ['a']      ["all"]             (NoArg RebuildAll)
      "Rebuild all Haskell libraries built with current GHC."
    , Option ['P']      ["package-manager"] (ReqArg PM "PM")
      $ "Use package manager PM, where PM can be one of:\n"
            ++ pmList ++ defPM
    , Option ['C']      ["custom-pm"]     (ReqArg CustomPMFlag "command")
      "Use custom command as package manager;\n\
      \ignores the --pretend and --no-deep flags."
    , Option ['p']      ["pretend"]         (NoArg Pretend)
      "Only pretend to build packages."
    , Option []         ["no-deep"]         (NoArg NoDeep)
      "Don't pull deep dependencies (--deep with emerge)."
    , Option ['l'] ["list-only"]            (NoArg ListOnlyFlag)
      "Output only list of packages for rebuild. One package per line."
    , Option ['V']      ["version"]         (NoArg VersionFlag)
      "Version information."
    , Option []          ["action"]          (ReqArg Cmd "action")
      $ "Specify whether to run the PM command or just print it\n"
            ++ actionList ++ defAction
    , Option ['q']      ["quiet"]           (NoArg QuietFlag)
      "Print only fatal errors (to stderr)."
    , Option ['v']      ["verbose"]         (NoArg VerboseFlag)
      "Be more elaborate (to stderr)."
    , Option ['h', '?'] ["help"]            (NoArg HelpFlag)
      "Print this help message."
    ]
    where
      pmList = unlines . map (" * " ++) $ definedPMs
      defPM = "The last valid value of PM specified is chosen.\n\
              \The default package manager is: " ++ defaultPMName ++ ",\n\
              \which can be overriden with the \"PACKAGE_MANAGER\"\n\
              \environment variable."
      actionList = unlines . map (" * " ++) $ M.keys withCmdMap
      defAction = "The last specified action is chosen.\n\
                       \The default action is: " ++ defaultWithCmd

-- -----------------------------------------------------------------------------
-- Printing information.

help :: IO a
help = progInfo >>= success Normal

version :: IO a
version = fmap (++ '-' : showVersion Paths.version) getProgName >>= success Normal

progInfo :: IO String
progInfo = do pName <- getProgName
              return $ usageInfo (header pName) options
  where
    header pName = unlines [ pName ++ " -- Find and rebuild packages broken due to either:"
                           , "            * GHC upgrade"
                           , "            * Haskell dependency upgrade"
                           , "         Default action is to do both."
                           , ""
                           , "Usage: " ++ pName ++ " [Options [-- [PM options]]"
                           , ""
                           , ""
                           , "Options:"]

systemInfo :: Verbosity -> RunModifier -> BuildTarget -> IO ()
systemInfo v rm t = do
    ver    <- ghcVersion
    pName  <- getProgName
    let pVer = showVersion Paths.version
    pLoc   <- ghcLoc
    libDir <- ghcLibDir
    say v $ "Running " ++ pName ++ "-" ++ pVer ++ " using GHC " ++ ver
    say v $ "  * Executable: " ++ pLoc
    say v $ "  * Library directory: " ++ libDir
    say v $ "  * Package manager (PM): " ++ nameOfPM (pkgmgr rm)
    unless (null (rawPMArgs rm)) $
        say v $ "  * PM auxiliary arguments: " ++ unwords (rawPMArgs rm)
    say v $ "  * Mode: " ++ show t
    say v ""

-- -----------------------------------------------------------------------------
-- Utility functions

success :: Verbosity -> String -> IO a
success v msg = do say v msg
                   exitSuccess

die     :: String -> IO a
die msg = do putErrLn ("ERROR: " ++ msg)
             exitWith (ExitFailure 1)

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

bool       :: a -> a -> Bool -> a
bool f t b = if b then t else f

partitionBy   :: (a -> Either l r) -> [a] -> ([l], [r])
partitionBy f = partitionEithers . map f

-- If the list is empty, return the provided value; otherwise use the function.
emptyElse        :: b -> ([a] -> b) -> [a] -> b
emptyElse e _ [] = e
emptyElse _ f as = f as
