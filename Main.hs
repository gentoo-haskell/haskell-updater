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
import qualified Distribution.Gentoo.CmdLine.Types as CmdLine -- (CmdLineArgs, BuildTarget)
import Distribution.Gentoo.GHC
import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager
import Distribution.Gentoo.Types as Types
import Distribution.Gentoo.Types.HUMode as Mode

import           Control.Monad         (unless)
import qualified Control.Monad         as CM
import           Data.Foldable         (toList)
import           Data.Sequence         (ViewR(..), viewr, (|>))
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
              Right (cmdArgs, rawArgs)  -> runAction cmdArgs rawArgs

runAction :: CmdLine.CmdLineArgs -> RawPMArgs -> IO a
runAction cmdArgs rawArgs = do
    mode <- either die pure $ mkHUMode cmdArgs rawArgs
    case mode of
        HelpMode -> help
        VersionMode -> version
        RunMode rm pm -> runUpdater rm pm rawArgs

dumpHistory :: Verbosity -> RunHistory -> IO ()
dumpHistory v historySeq = do
    say v "Updater's past history:"
    CM.forM_ historyList $ \(n, entry, ec) -> say v $ unwords
        [ "Pass"
        , show n ++ ":"
        , show $ printPkg <$> Set.toList entry
        , show ec
        ]
  where historyList :: [(Int, Set.Set Package, ExitCode)]
        historyList =
            [ (n, entry, ec)
            | ((entry, ec), n) <- zip (toList historySeq) [1..]
            ]

type UpdaterLoop
    =  PendingPackages
    -> Set.Set Types.Target
    -> BuildPkgs
    -> RunHistory
    -> IO ()

-- | Run the main part of @haskell-updater@ (e.g. not @--help@ or
--   @--version@).
runUpdater
    :: RunModifier
    -> PkgManager
    -> RawPMArgs
    -> IO a
runUpdater rm pkgMgr userArgs = do
    systemInfo rm pkgMgr userArgs
    (ps, ts, bps) <- getPackageState v pkgMgr
    case runMode pkgMgr of
        Left (ListMode _) -> listPkgs ps
        Right (PortageListMode _) -> listPkgs ps
        _ -> case getLoopType pkgMgr of
            UntilNoPending -> loopUntilNoPending ps ts bps Seq.empty
            UntilNoChange -> loopUntilNoChange ps ts bps Seq.empty
            NoLoop -> buildPkgs rm rawArgs ps ts bps >>= exitWith
    success v "done!"
  where
    listPkgs :: PendingPackages -> IO ()
    listPkgs ps = do
        mapM_ (putStrLn . printPkg) (getPkgs ps)
        success v "done!"

    loopUntilNoPending :: UpdaterLoop
    loopUntilNoPending ps ts bps hist
        | getPkgs ps `elem` (fst <$> toList hist) = alertStuck hist
        | Set.null (getPkgs ps) = alertDone
        | otherwise = next loopUntilNoPending ps ts bps hist

    loopUntilNoChange :: UpdaterLoop
    loopUntilNoChange ps ts bps hist = case viewr hist of
        EmptyR -> next loopUntilNoChange ps ts bps hist
        (_ :> (past, pastEC))
            | getPkgs ps == past -> case pastEC of
                ExitFailure _ -> alertStuck hist
                ExitSuccess -> alertDone
            | otherwise -> next loopUntilNoChange ps ts bps hist

    next :: UpdaterLoop -> UpdaterLoop
    next f ps ts bps hist = do
        exitCode <- buildPkgs rm rawArgs ps ts bps

        let hist' = hist |> (getPkgs ps, exitCode)
        (ps', ts', bps') <- getPackageState v pkgMgr

        f ps' ts' bps' hist'

    alertDone = success (verbosity rm) "\nNothing to build!"
    alertStuck hist = do
        dumpHistory v hist
        die "Updater stuck in the loop and can't progress"

    rawArgs = getExtraRawArgs pkgMgr
    v = verbosity rm

-- | Determines which function 'buildPkgs' will run to get the package-manager
--   command.
data BuildPkgs
    = BuildNormal Mode.PkgManager -- ^ @--mode=basic@
    | BuildRAMode AllPkgs -- ^ @--mode=reinstall-atoms@

-- | As needed, query @ghc-pkg check@ for broken packages, scan the filesystem
--   for installed packages, and look for misc breakages. Return the results
--   summarized for use with 'buildPkgs'.
getPackageState
    :: Verbosity
    -> PkgManager
    -> IO (PendingPackages, Set.Set Types.Target, BuildPkgs)
getPackageState v pkgMgr =
    case runMode pkgMgr of
        Left mode -> fromRunMode mode
        Right (PortageBasicMode (Left PreservedRebuild)) -> do
            p <- InvalidPending <$> getInvalid
            let s = Set.singleton $ CustomTarget "@preserved-rebuild"
            pure (p, s, BuildNormal pkgMgr)
        Right (PortageBasicMode (Right targ)) -> fromRunMode (BasicMode targ)
        Right (PortageListMode targ) -> fromRunMode (ListMode targ)
        Right (ReinstallAtomsMode targ) -> do
            aps <- getAll
            (p,ts) <- case targ of
                Left OnlyInvalid -> do
                    ips <- getInvalid
                    pure (InvalidPending ips, Set.singleton (TargetInvalid ips))
                Left AllInstalled ->
                    pure (AllPending aps, Set.singleton (TargetAll aps))
                Right t -> do
                    ips <- getInvalid
                    let ts = case t of
                            WorldTarget -> Set.singleton (CustomTarget "@world")
                            WorldFullTarget -> Set.singleton (CustomTarget "@world")
                            CustomTargets cts -> Set.fromList (CustomTarget <$> cts)
                    pure (InvalidPending ips, ts)
            pure (p, ts, BuildRAMode aps)
  where
    fromRunMode
        :: RunMode
        -> IO (PendingPackages, Set.Set Types.Target, BuildPkgs)
    fromRunMode mode = do
        p <- case getTarget mode of
            OnlyInvalid -> InvalidPending <$> getInvalid
            AllInstalled -> AllPending <$> getAll
        pure (p, Set.empty, BuildNormal pkgMgr)

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

buildPkgs
    :: RunModifier
    -> ExtraRawArgs
    -> PendingPackages
    -> Set.Set Types.Target
    -> BuildPkgs
    -> IO ExitCode
buildPkgs rm rawArgs pps targets bp = do
    putStrLn ""
    runCmd (withCmd rm) cmd args
  where
    (cmd, args) = case bp of
        BuildNormal pkgMgr ->
            buildCmd pkgMgr (flags rm) rawArgs (rawPMArgs rm) pps targets
        BuildRAMode allPkgs ->
            buildRACmd (flags rm) rawArgs (rawPMArgs rm) pps targets allPkgs

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

systemInfo
    :: RunModifier
    -> PkgManager
    -> RawPMArgs
    -> IO ()
systemInfo rm pkgMgr rawArgs = do
    ver    <- ghcVersion
    pName  <- getProgName
    let pVer = showVersion Paths.version
    pLoc   <- ghcLoc
    libDir <- ghcLibDir
    say v $ "Running " ++ pName ++ "-" ++ pVer ++ " using GHC " ++ ver
    say v $ "  * Executable: " ++ pLoc
    say v $ "  * Library directory: " ++ libDir
    say v $ "  * Package manager (PM): " ++ nameOfPM (toPkgManager pkgMgr)
    unless (null rawArgs) $
        say v $ "  * PM auxiliary arguments: " ++ unwords rawArgs
    say v $ "  * Targets: " ++ ts
    say v $ "  * Mode: " ++ argString m
    say v ""
  where
    v = verbosity rm

    (m, ts) = case runMode pkgMgr of
        Left mode -> printRunMode mode
        Right (PortageBasicMode (Left PreservedRebuild)) ->
            (CmdLine.BasicMode, argString CmdLine.PreservedRebuild)
        Right (PortageBasicMode (Right targ)) ->
            printRunMode (BasicMode targ)
        Right (PortageListMode targ) ->
            printRunMode (ListMode targ)
        Right (ReinstallAtomsMode targ) -> case targ of
            Left OnlyInvalid ->
                (CmdLine.ReinstallAtomsMode, argString CmdLine.OnlyInvalid)
            Left AllInstalled ->
                (CmdLine.ReinstallAtomsMode, argString CmdLine.AllInstalled)
            Right WorldTarget ->
                (CmdLine.ReinstallAtomsMode, argString CmdLine.WorldTarget)
            Right WorldFullTarget ->
                (CmdLine.ReinstallAtomsMode, unwords
                    [argString CmdLine.WorldTarget, "(full)"])
            Right (CustomTargets cts) ->
                (CmdLine.ReinstallAtomsMode, unwords cts)

    printRunMode :: RunMode -> (CmdLine.RunMode, String)
    printRunMode = \case
        BasicMode OnlyInvalid ->
            (CmdLine.BasicMode, argString CmdLine.OnlyInvalid)
        BasicMode AllInstalled ->
            (CmdLine.BasicMode, argString CmdLine.AllInstalled)
        ListMode OnlyInvalid ->
            (CmdLine.ListMode, argString CmdLine.OnlyInvalid)
        ListMode AllInstalled ->
            (CmdLine.ListMode, argString CmdLine.AllInstalled)

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
