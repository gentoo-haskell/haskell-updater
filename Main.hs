{- |
   Module      : Main
   Description : The haskell-updater executable
   Copyright   : (c) Ivan Lazar Miljenovic, Stephan Friedrichs, Emil Karlson 2010
   License     : GPL-2 or later

   The executable module of haskell-updater, which finds Haskell
   packages to rebuild after a dep upgrade or a GHC upgrade.
-}

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Distribution.Gentoo.CmdLine
import Distribution.Gentoo.GHC
import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager
import Distribution.Gentoo.PkgManager.Types
import Distribution.Gentoo.Types

import           Control.Monad         (unless)
import qualified Control.Monad         as CM
import           Data.Foldable         (toList)
import qualified Data.List             as L
import           Data.Sequence         (Seq, ViewR(..), viewr)
import qualified Data.Sequence         as Seq
import qualified Data.Set              as Set
import           Data.Version          (showVersion)
import qualified Paths_haskell_updater as Paths (version)
import           System.Console.GetOpt
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (ExitCode (..), exitSuccess, exitWith)
import           System.IO             (hPutStrLn, stderr)
import           System.Process        (rawSystem)

import Output

main :: IO ()
main = do args <- getArgs
          defPM <- defaultPM
          case parseArgs defPM args of
              Left err -> die err
              Right a  -> runAction a

runAction :: RunModifier -> IO a
runAction rm
    | showHelp rm    = help
    | showVer rm     = version
    | otherwise      = runDriver rm

-- set of packages to rebuild at pass number
type DriverHistory = Seq (Set.Set Package, ExitCode)

data TargetType
    = InvalidTargets
    | AllTargets
    | UpdateTargets

initialHistory :: DriverHistory
initialHistory = Seq.empty

dumpHistory :: Verbosity -> DriverHistory -> IO ()
dumpHistory v historySeq = do
    say v "Updater's past history:"
    CM.forM_ historyList $ \(n, entry, ec) ->
      say v $ unwords $ ["Pass", show n, ":"] ++ map printPkg (Set.toList entry) ++ [show ec]
  where historyList :: [(Int, Set.Set Package, ExitCode)]
        historyList = L.sort [ (n, entry, ec) | ((entry, ec), n) <- zip (toList historySeq) [1..] ]

runDriver :: RunModifier -> IO a
runDriver rm = do
    systemInfo v rm t md
    updaterPass initialHistory
    success v "done!"
  where
    v = verbosity rm
    t = target rm
    md = mode rm

    updaterPass :: DriverHistory -> IO ()
    updaterPass pastHistory = getPackageState rm >>= \case
        ListModeState m -> do
            mapM_ (putStrLn . printPkg) (targetPkgs m)
            success v "done!"
        DefaultModeState (Just (DefaultInvalid ts)) ->
            continuePass (Left . DefaultInvalid) InvalidTargets ts
        DefaultModeState (Just (DefaultAll ts)) ->
            continuePass (Left . DefaultAll) AllTargets ts
        DefaultModeState Nothing -> alertDone
        RAModeState allPs (RAModeInvalid ts) ->
            continuePass
                (\ps -> Right (RAModeInvalid ps, allPs))
                UpdateTargets
                ts
        RAModeState allPs RAModeAll ->
            continuePass
                (\_ -> Right (RAModeAll, allPs))
                AllTargets
                ()
        RAModeState allPs (RAModeWorld ts) ->
            continuePass
                (\ps -> Right (RAModeWorld ps, allPs))
                UpdateTargets
                ts

      where

        alertDone = success (verbosity rm) "\nNothing to build!"

        continuePass
            :: PackageSet ts
            => (ts -> Either DefaultModePkgs (RAModePkgs, AllPkgs))
            -> TargetType
            -> ts
            -> IO ()
        continuePass cnst targetType ts = do
            let next ec = updaterPass
                    $ pastHistory <> Seq.singleton (getPkgs ts, ec)

            case targetType of
                InvalidTargets -> do
                    CM.when (getPkgs ts `elem` map fst (toList pastHistory)) $ do
                        dumpHistory v pastHistory
                        die "Updater stuck in the loop and can't progress"

                    exitCode <- buildPkgs rm (cnst ts)

                    next exitCode
                AllTargets -> do
                    exitCode <- buildPkgs rm (cnst ts)

                    exitWith exitCode
                UpdateTargets -> case viewr pastHistory of
                    EmptyR -> buildPkgs rm (cnst ts) >>= next
                    (_ :> (past, pastEC)) -> do
                        case (getPkgs ts == past, pastEC) of
                            (True, ExitFailure _) -> do
                                dumpHistory v pastHistory
                                die "Updater stuck in the loop and can't progress"
                            (True, ExitSuccess) -> exitSuccess
                            _ -> buildPkgs rm (cnst ts) >>= next

getPackageState :: RunModifier -> IO PackageState
getPackageState rm =
    case (mode rm, target rm, pkgmgr rm) of
        -- world target
        (ReinstallAtomsMode, WorldTarget, Portage) -> do
            is <- getInvalid
            allPs <- getAll
            pure $ RAModeState allPs $ RAModeWorld is
        (_, WorldTarget, Portage) -> die
            "\"world\" target is only valid with reinstall-atoms mode"
        (ReinstallAtomsMode, WorldTarget, _) -> die
            "\"world\" target is only valid with portage package manager"
        (_, WorldTarget, _) -> die $ unwords
            ["\"world\" target is only valid with reinstall-atoms mode and portage"
            , "package manager"]
        -- list mode
        (ListMode, OnlyInvalid, _) ->
            ListModeState . ListInvalid <$> getInvalid
        (ListMode, AllInstalled, _) ->
            ListModeState . ListAll <$> getAll
        -- default mode
        (BasicMode, OnlyInvalid, _) ->
            DefaultModeState . checkForNull DefaultInvalid <$> getInvalid
        (BasicMode, AllInstalled, _) ->
            DefaultModeState . checkForNull DefaultAll <$> getAll
        -- reinstall-atoms mode
        (ReinstallAtomsMode, OnlyInvalid, Portage) -> do
            is <- getInvalid
            allPs <- getAll
            pure $ RAModeState allPs $ RAModeInvalid is
        (ReinstallAtomsMode, AllInstalled, Portage) -> do
            allPs <- getAll
            pure $ RAModeState allPs RAModeAll
        (ReinstallAtomsMode, _, _) -> die
            "reinstall-atoms mode is only valid with portage package manager"
  where
    getInvalid = do
        say v "Searching for packages installed with a different version of GHC."
        say v ""
        old <- oldGhcPkgs v
        pkgListPrintLn v "old" old

        say v "Searching for Haskell libraries with broken dependencies."
        say v ""
        (broken, unknown_packages, unknown_files) <- brokenPkgs v
        let broken' = Set.fromList broken
        printUnknownPackagesLn (map unCPV unknown_packages)
        printUnknownFilesLn unknown_files
        pkgListPrintLn v "broken" (notGHC broken')

        return $ InvalidPkgs $ old <> broken'

    getAll = do
        say v "Searching for packages installed with the current version of GHC."
        say v ""
        pkgs <- allInstalledPackages
        pkgListPrintLn v "installed" pkgs
        return $ AllPkgs pkgs

    checkForNull :: PackageSet ts => (ts -> b) -> ts -> Maybe b
    checkForNull cnst l
        | null (getPkgs l) = Nothing
        | otherwise = Just (cnst l)

    printUnknownPackagesLn [] = return ()
    printUnknownPackagesLn ps = do
        say v "The following packages are orphan (not installed by your package manager):"
        printList v id ps
        say v ""
    printUnknownFilesLn [] = return ()
    printUnknownFilesLn fs = do
        say v $ "The following files are orphan (not installed by your package manager):"
        printList v id fs
        say v $ "It is strongly advised to remove orphans:"
        say v $ "    One of known sources of orphans is packages installed before 01 Jan 2015."
        say v $ "    If you know it's your case you can easily remove such files:"
        say v $ "        # rm -v -- `qfile -o $(ghc --print-libdir)/package.conf.d/*.conf $(ghc --print-libdir)/gentoo/*.conf`"
        say v $ "        # ghc-pkg recache"
        say v $ "    It will likely need one more 'haskell-updater' run."
        say v ""

    v = verbosity rm

runCmd :: WithCmd -> String -> [String] -> IO ExitCode
runCmd m cmd args = case m of
        RunOnly     ->                      runCommand cmd args
        PrintOnly   -> putStrLn cmd_line >> exitSuccess
        PrintAndRun -> putStrLn cmd_line >> runCommand cmd args
  where
    cmd_line = unwords (cmd : (showArg <$> args))
    showArg s
        | words s == [s] = s
        | otherwise = show s -- Put quotes around args with spaces in them

runCommand     :: String -> [String] -> IO ExitCode
runCommand cmd args = rawSystem cmd args

buildPkgs :: RunModifier -> Either DefaultModePkgs (RAModePkgs, AllPkgs) -> IO ExitCode
buildPkgs rm ts = runCmd (withCmd rm) cmd args
  where
    (cmd, args) = case ts of
        Left ps ->
            buildCmd (pkgmgr rm) (flags rm) (rawPMArgs rm) ps
        Right (ps, allPkgs) ->
            buildAltCmd (flags rm) (rawPMArgs rm) ps allPkgs

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
    header pName = unlines [ pName ++ " -- Find and rebuild packages broken due to any of:"
                           , "            * GHC upgrade"
                           , "            * Haskell dependency upgrade"
                           , ""
                           , "Usage: " ++ pName ++ " [Options [-- [PM options]]"
                           , ""
                           , ""
                           , "Options:"]

systemInfo :: Verbosity -> RunModifier -> BuildTarget -> HackportMode -> IO ()
systemInfo v rm t m = do
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
    say v $ "  * Target: " ++ argString t
    say v $ "  * Mode: " ++ argString m
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
