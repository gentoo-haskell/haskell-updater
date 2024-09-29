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
import Distribution.Gentoo.Util (These(..), toListNE)

import           Control.Monad         (unless)
import qualified Control.Monad         as CM
import           Control.Monad.State   (StateT, evalStateT, get, put, liftIO)
import           Data.Bifoldable       (bifoldMap)
import           Data.Foldable         (toList)
import qualified Data.List             as L
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
    say v ""
  where historyList :: [(Int, Set.Set Package, ExitCode)]
        historyList =
            [ (n, entry, ec)
            | ((entry, ec), n) <- zip (toList historySeq) [1..]
            ]

type UpdaterLoop
    =  BuildPkgs
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
    bps <- getPackageState v pkgMgr
    let ps = buildPkgsPending bps
    case runMode pkgMgr of
        Left (ListMode _) -> listPkgs ps
        Right (PortageListMode _) -> listPkgs ps
        _ -> case getLoopType pkgMgr of
            UntilNoPending -> loopUntilNoPending bps Seq.empty
            UntilNoChange -> loopUntilNoChange bps Seq.empty
            NoLoop -> buildPkgs rm rawArgs bps >>= exitWith
    success v "done!"
  where
    listPkgs :: PendingPackages -> IO ()
    listPkgs ps = do
        mapM_ (putStrLn . printPkg) (getPkgs ps)
        success v "done!"

    loopUntilNoPending :: UpdaterLoop
    loopUntilNoPending bps hist
        | getPkgs ps `elem` (fst <$> toList hist) = alertStuck hist Nothing
        | Set.null (getPkgs ps) = alertDone
        | otherwise = next loopUntilNoPending bps hist
        where ps = buildPkgsPending bps

    loopUntilNoChange :: UpdaterLoop
    loopUntilNoChange bps hist = case viewr hist of
        EmptyR -> next loopUntilNoChange bps hist
        (prevHist :> (lastPkgSet, lastEC))
            | getPkgs ps == lastPkgSet -> case lastEC of
                ExitFailure _
                    -- Did it fail after one pass?
                    | Seq.null prevHist -> alertStuck hist (Just stuckOnePass)
                    | otherwise -> alertStuck hist Nothing
                ExitSuccess -> alertDone
            | otherwise -> next loopUntilNoChange bps hist
      where
        ps = buildPkgsPending bps

        -- This mostly comes up with the 'UntilNoChange' loop type, so we'll limit
        -- it to that for now.
        stuckOnePass =
            [ "NOTE: The updater loop failed after one pass, with no changes to the state of"
            , "broken Haskell packages on the system. This is often caused by the package"
            , "manager failing during dependency resolution. Check the output to be sure."
            ]

    next :: UpdaterLoop -> UpdaterLoop
    next f bps hist = do
        exitCode <- buildPkgs rm rawArgs bps

        bps' <- getPackageState v pkgMgr
        let ps = buildPkgsPending bps'
            hist' = hist |> (getPkgs ps, exitCode)

        f bps' hist'

    alertDone = success (verbosity rm) "Nothing to build!"

    alertStuck hist maybeMsg = do
        dumpHistory v hist
        die $ unlines
            $ "Updater stuck in the loop and can't progress"
            : maybe [] ("":) maybeMsg

    rawArgs = getExtraRawArgs pkgMgr
    v = verbosity rm

-- | Determines which function 'buildPkgs' will run to get the package-manager
--   command.
data BuildPkgs
    -- | Default mode
    = BuildNormal
        -- | the package manager that will be used
        Mode.PkgManager
        -- | Packages that will be rebuilt, passed to the PM as targets
        PendingPackages
        -- | Extra targets
        (Set.Set Types.Target)
    -- | @--mode=reinstall-atoms@
    | BuildRAMode
        -- | Packages that will be marked for rebuild via --reinstall-atoms
        PendingPackages
        -- | atoms/sets that the PM will be targeting
        (Set.Set Types.Target)
        -- | All installed Haskell packages (for use with @--usepkg-exclude@)
        AllPkgs

-- | The set of targets that will be passed to the package manager. This mostly
--   matters for 'BuildNormal', since there are two sets that must be merged
--   for the final target set.
buildPkgsTargets :: BuildPkgs -> Set.Set Types.Target
buildPkgsTargets = \case
    BuildNormal _ pps extraTargs ->
        let pts = Set.singleton $ case pps of
                InvalidPending ps -> TargetInvalid ps
                AllPending as -> TargetAll as
        in pts <> extraTargs
    BuildRAMode _ targs _ -> targs

-- | Get the 'PendingPackages' from a 'BuildPkgs' constructor.
--
--   This is examined by the different looping strategies in order to monitor
--   progress and make choices about when to continue looping.
buildPkgsPending :: BuildPkgs -> PendingPackages
buildPkgsPending = \case
    BuildNormal _ pps _ -> pps
    BuildRAMode pps _ _ -> pps

-- | As needed, query @ghc-pkg check@ for broken packages, scan the filesystem
--   for installed packages, and look for misc breakages. Return the results
--   summarized for use with 'buildPkgs'.
getPackageState
    :: Verbosity
    -> PkgManager
    -> IO BuildPkgs
getPackageState v pkgMgr =
    case runMode pkgMgr of
        Left mode -> fromRunMode mode
        Right (PortageBasicMode (Left PreservedRebuild)) -> do
            ps <- InvalidPending <$> getInvalid
            let ct = CustomTarget "@preserved-rebuild"
            pure $ BuildNormal pkgMgr ps (Set.singleton ct)
        Right (PortageBasicMode (Right targ)) -> fromRunMode (BasicMode targ)
        Right (PortageListMode targ) -> fromRunMode (ListMode targ)
        Right (ReinstallAtomsMode targ) -> flip evalStateT Nothing $ do
            aps <- liftIO getAll
            let pending = \case
                    OnlyInvalid -> withIPCache InvalidPending
                    AllInstalled -> pure $ AllPending aps
                normalTarg = \case
                    OnlyInvalid -> withIPCache TargetInvalid
                    AllInstalled -> pure $ TargetAll aps
                extraTargs = bifoldMap
                            (Set.singleton . \case
                                WorldTarget -> CustomTarget "@world"
                                WorldFullTarget -> CustomTarget "@world"
                            )
                            (Set.fromList . map CustomTarget . toListNE)
            (p,ts) <- case targ of
                These ta th -> do
                    p <- pending ta
                    nt <- normalTarg ta
                    pure (p, Set.insert nt (extraTargs th))
                This ta -> do
                    p <- pending ta
                    nt <- normalTarg ta
                    pure (p, Set.singleton nt)
                That th -> do
                    p <- InvalidPending <$> liftIO getInvalid
                    pure (p, extraTargs th)
            pure $ BuildRAMode p ts aps
  where
    fromRunMode
        :: RunMode
        -> IO BuildPkgs
    fromRunMode mode = do
        ps <- case getTarget mode of
            OnlyInvalid -> InvalidPending <$> getInvalid
            AllInstalled -> AllPending <$> getAll
        pure $ BuildNormal pkgMgr ps Set.empty

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

    withIPCache
        :: (InvalidPkgs -> a)
        -> StateT (Maybe InvalidPkgs) IO a
    withIPCache f = fmap f $ get >>= \case
        Nothing -> do
            ips <- liftIO getInvalid
            put (Just ips)
            pure ips
        Just ips -> pure ips

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
    -> BuildPkgs
    -> IO ExitCode
buildPkgs rm rawArgs bp = do
    putStrLn ""
    runCmd (withCmd rm) cmd args
  where
    (cmd, args) = case bp of
        BuildNormal pkgMgr _ _ ->
            let targs = buildPkgsTargets bp
            in buildCmd pkgMgr (flags rm) rawArgs (rawPMArgs rm) targs
        BuildRAMode pps targs allPkgs ->
            buildRACmd (flags rm) rawArgs (rawPMArgs rm) pps targs allPkgs

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
    say v $ "  * Targets: " ++ L.intercalate ", " ts
    say v $ "  * Mode: " ++ argString m
    say v ""
  where
    v = verbosity rm

    (m, ts) = case runMode pkgMgr of
        Left mode -> (:[]) <$> printRunMode mode
        Right (PortageBasicMode (Left PreservedRebuild)) ->
            (CmdLine.BasicMode, [argString CmdLine.PreservedRebuild])
        Right (PortageBasicMode (Right targ)) ->
            (:[]) <$> printRunMode (BasicMode targ)
        Right (PortageListMode targ) ->
            (:[]) <$> printRunMode (ListMode targ)
        Right (ReinstallAtomsMode targ) ->
            let strs = bifoldMap
                    ((:[]) . \case
                        OnlyInvalid -> argString CmdLine.OnlyInvalid
                        AllInstalled -> argString CmdLine.AllInstalled
                    )
                    (bifoldMap
                        ((:[]) . \case
                            WorldTarget -> argString CmdLine.WorldTarget
                            WorldFullTarget -> unwords
                                [argString CmdLine.WorldTarget, "(full)"]
                        )
                        toListNE
                    )
                    targ
            in (CmdLine.ReinstallAtomsMode, strs)

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
