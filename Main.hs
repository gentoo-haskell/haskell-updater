{- |
   Module      : Main
   Description : The haskell-updater executable
   Copyright   : (c) Ivan Lazar Miljenovic, Stephan Friedrichs, Emil Karlson 2010
   License     : GPL-2 or later

   The executable module of haskell-updater, which finds Haskell
   packages to rebuild after a dep upgrade or a GHC upgrade.
-}
module Main where

import Distribution.Gentoo.GHC
import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager
import Distribution.Gentoo.Util

import Data.Either(partitionEithers)
import Data.List(foldl1', nub)
import Data.Version(showVersion)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Paths_haskell_updater as Paths(version)
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import System.Exit(ExitCode(..), exitWith)
import System.IO(hPutStrLn, stderr)
import Control.Monad(liftM, unless)
import System.Process(rawSystem)

-- -----------------------------------------------------------------------------
-- The overall program.

main :: IO ()
main = uncurry runAction =<< parseArgs

-- -----------------------------------------------------------------------------
-- The possible actions that haskell-updater can perform.

data Action = Help
            | Version
            | Build { targets :: Set BuildTarget }
              -- If anything is added here after Build, MAKE SURE YOU
              -- UPDATE combineActions or it won't always work!
              deriving (Eq, Ord, Show, Read)

defaultAction :: Action
defaultAction = Build $ Set.fromList [GhcUpgrade, DepCheck]

-- Combine all the actions together.  If the list is empty, use the
-- defaultAction.
combineAllActions :: [Action] -> Action
combineAllActions = emptyElse defaultAction (foldl1' combineActions)

-- Combine two actions together.  If they're both Build blah, merge
-- them; otherwise, pick the lower of the two (i.e. more important).
-- Note that it's safe (at the moment at least) to assume that when
-- the lower of one is a Build that they're both build.
combineActions       :: Action -> Action -> Action
combineActions a1 a2 = case (a1 `min` a2) of
                         Build{} -> Build $ targets a1 `Set.union` targets a2
                         a       -> a

runAction               :: RunModifier -> Action -> IO a
runAction _  Help       = help
runAction _  Version    = version
runAction rm (Build ts) = do systemInfo rm
                             ps <- allGetPackages ts
                             buildPkgs rm ps

-- -----------------------------------------------------------------------------
-- The possible things to build.

data BuildTarget = GhcUpgrade
                 | DepCheck
                 | AllInstalled
                   deriving (Eq, Ord, Show, Read)

getPackages              :: BuildTarget -> IO [Package]
getPackages GhcUpgrade   = oldGhcPkgs
getPackages DepCheck     = brokenPkgs
getPackages AllInstalled = allInstalledPackages

allGetPackages :: Set BuildTarget -> IO [Package]
allGetPackages = liftM nub
                   . concatMapM getPackages
                   . Set.toList

-- -----------------------------------------------------------------------------
-- How to build packages.

data RunModifier = RM { pkgmgr   :: PkgManager
                      , flags    :: [PMFlag]
                      , withCmd  :: WithCmd
                      , rawPMArgs :: [String]
                      }
                   deriving (Eq, Ord, Show, Read)

-- At the moment, PrintAndRun is the only option available.
data WithCmd = RunOnly
             | PrintOnly
             | PrintAndRun
               deriving (Eq, Ord, Show, Read)

runCmd :: WithCmd -> String -> [String] -> IO a
runCmd mode cmd args = case mode of
        RunOnly     ->                      runCommand cmd args
        PrintOnly   -> success cmd_line
        PrintAndRun -> putStrLn cmd_line >> runCommand cmd args
    where cmd_line = unwords (cmd:args)

runCommand     :: String -> [String] -> IO a
runCommand cmd args = rawSystem cmd args >>= exitWith

buildPkgs       :: RunModifier -> [Package] -> IO a
buildPkgs _  [] = success "\nNothing to build!"
buildPkgs rm ps = runCmd (withCmd rm) cmd args
    where
      (cmd, args) = buildCmd (pkgmgr rm) (flags rm) (rawPMArgs rm) ps

-- -----------------------------------------------------------------------------
-- Command-line flags

data Flag = HelpFlag
          | VersionFlag
          | PM String
          | CustomPMFlag String
          | Check
          | Upgrade
          | RebuildAll
          | Pretend
          | NoDeep
          deriving (Eq, Ord, Show, Read)

parseArgs :: IO (RunModifier, Action)
parseArgs = do args <- getArgs
               defPM <- defaultPM
               argParser defPM $ getOpt' Permute options args

argParser                    :: PkgManager -> ([Flag], [String], [String], [String])
                                -> IO (RunModifier, Action)
argParser dPM (fls, nonoptions, unrecognized, []) =
    do unless (null unrecognized)
         $ putErrLn
         $ unwords $ "Unknown options:" : unrecognized
       unless (null bPms)
         $ putErrLn
         $ unwords $ "Unknown package managers:" : bPms
       return (rm, a)
    where
      (fls', as) = partitionBy flagToAction fls
      a = combineAllActions as
      (opts, pms) = partitionBy flagToPM fls'
      (bPms, pms') = partitionBy isValidPM pms
      pm = emptyElse dPM last pms'
      opts' = Set.fromList opts
      hasFlag = flip Set.member opts'
      pmFlags = bool id (PretendBuild:) (hasFlag Pretend)
                . return $ bool UpdateDeep UpdateAsNeeded (hasFlag NoDeep)
      rm = RM { pkgmgr   = pm
              , flags    = pmFlags
                -- We need to get Flags that represent this as well.
              , withCmd  = PrintAndRun
              , rawPMArgs = nonoptions
              }

argParser _ (_, _, _, errs)     = die $ unwords $ "Errors in arguments:" : errs

flagToAction             :: Flag -> Either Flag Action
flagToAction HelpFlag    = Right Help
flagToAction VersionFlag = Right Version
flagToAction Check       = Right . Build $ Set.singleton DepCheck
flagToAction Upgrade     = Right . Build $ Set.singleton GhcUpgrade
flagToAction RebuildAll  = Right . Build $ Set.singleton AllInstalled
flagToAction f           = Left f

flagToPM                   :: Flag -> Either Flag PkgManager
flagToPM (CustomPMFlag pm) = Right $ stringToCustomPM pm
flagToPM (PM pm)           = Right $ choosePM pm
flagToPM f                 = Left f

options :: [OptDescr Flag]
options =
    [ Option ['c']      ["dep-check"]       (NoArg Check)
      "Check dependencies of Haskell packages."
    , Option ['u']      ["upgrade"]         (NoArg Upgrade)
      "Rebuild Haskell packages after a GHC upgrade."
    , Option []         ["all"]             (NoArg RebuildAll)
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
    , Option ['v']      ["version"]         (NoArg VersionFlag)
      "Version information."
    , Option ['h', '?'] ["help"]            (NoArg HelpFlag)
      "Print this help message."
    ]
    where
      pmList = unlines . map ((++) "  * ") $ definedPMs
      defPM = "The last valid value of PM specified is chosen.\n\
              \The default package manager is: " ++ defaultPMName ++ ",\n\
              \which can be overriden with the \"PACKAGE_MANAGER\"\n\
              \environment variable."

-- -----------------------------------------------------------------------------
-- Printing information.

help :: IO a
help = progInfo >>= success

version :: IO a
version = fmap (++ '-' : showVersion Paths.version) getProgName >>= success

err     :: String -> IO a
err msg = liftM addMsg progInfo >>= die
  where
    addMsg str = msg ++ "\n\n"++ str

progInfo :: IO String
progInfo = do pName <- getProgName
              return $ usageInfo (header pName) options
  where
    header pName = pName ++ " -- Find and rebuild packages broken due to either:\n\
                  \            * GHC upgrade\n\
                  \            * Haskell dependency upgrade\n\
                  \         Default action is to do both.\n\
                  \\n\
                  \Usage: " ++ pName ++ " [Options [-- [PM options]]\n\
                  \\n\
                  \\n\
                  \Options:"

systemInfo    :: RunModifier -> IO ()
systemInfo rm = do ver    <- ghcVersion
                   pName  <- getProgName
                   pLoc   <- ghcLoc
                   libDir <- ghcLibDir
                   putStrLn $ "Running " ++ pName ++ " using GHC " ++ ver
                   putStrLn $ "  * Executable: " ++ pLoc
                   putStrLn $ "  * Library directory: " ++ libDir
                   putStrLn $ "  * Package manager (PM): " ++ nameOfPM (pkgmgr rm)
                   unless (null (rawPMArgs rm)) $
                       putStrLn $ "  * PM auxiliary arguments: " ++ unwords (rawPMArgs rm)
                   putStrLn ""

-- -----------------------------------------------------------------------------
-- Utility functions

success     :: String -> IO a
success msg = do putStrLn msg
                 exitWith ExitSuccess

die     :: String -> IO a
die msg = do putErrLn msg
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
