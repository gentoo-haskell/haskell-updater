{- |
   Module      : Main
   Description : The haskell-updater executable
   Copyright   : (c) Ivan Lazar Miljenovic, Stephan Friedrichs, Emil Karlson 2010
   License     : GPL-2 or later

   The executable module of haskell-updater, which finds Haskell
   packages to rebuild after a dep upgrade or a GHC upgrade.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Distribution.Gentoo.CmdLine
import qualified Distribution.Gentoo.CmdLine.Types as CmdLine -- (CmdLineArgs, BuildTarget)
import Distribution.Gentoo.Env
import Distribution.Gentoo.GHC
    ( MonadPkgState (oldGhcPkgs, brokenPkgs, allInstalledPkgs)
    , ghcVersion, ghcLibDir, ghcLoc, unCPV
    )
import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager
import Distribution.Gentoo.Types as Types
import Distribution.Gentoo.Types.HUMode as Mode
import Distribution.Gentoo.Util (These(..), toListNE)

import           Control.Monad         (unless, when)
import qualified Control.Monad         as CM
import           Control.Monad.State.Strict
    (MonadState, StateT, evalStateT, get, put, MonadIO, liftIO)
import           Data.Bifoldable       (bifoldMap)
import           Data.Foldable         (toList)
import qualified Data.List             as L
import           Data.Proxy
import           Data.Sequence         ((|>))
import qualified Data.Sequence         as Seq
import qualified Data.Set              as Set
import           Data.Version          (showVersion)
import qualified Paths_haskell_updater as Paths (version)
import           System.Console.GetOpt
import           System.Environment    (getArgs, getProgName)

import Output

main :: IO ()
main = do args <- getArgs
          defPM <- defaultPM
          case parseArgs defPM args of
              Left err -> die err
              Right (cmdArgs, rawArgs)  -> runAction cmdArgs rawArgs

runAction :: CmdLine.CmdLineArgs -> RawPMArgs -> IO ()
runAction cmdArgs rawArgs = do

    mode <- either die pure $ mkHUMode cmdArgs rawArgs
    case mode of
        HelpMode -> help
        VersionMode -> version
        RunMode rm pm -> flip runEnvT (rm, pm, rawArgs) $ do

            vsay "Command line args:"
            liftIO getArgs >>= vsay . show
            vsay $ show cmdArgs
            vsay ""
            vsay "Internal representation for haskell-updater mode:"
            vsay $ show (RunMode rm pm)
            vsay ""
            vsay "Looping strategy:"
            vsay $ show (getLoopType pm)
            vsay ""

            systemInfo pm rawArgs

            bps <- getPackageState
            let ps = buildPkgsPending bps

            case runMode pm of
                 Left (ListMode _) -> listPkgs ps
                 Right (PortageListMode _) -> listPkgs ps
                 _ -> runIOEnv runUpdater
                            (bps, RunHistory ps Seq.empty)
  where
    listPkgs ps = do
        mapM_ (liftIO . putStrLn . printPkg) (getPkgs ps)
        success "done!"

dumpHistory :: forall m. (MonadSay m, Show (ExitArg m))
    => RunHistory m -> m ()
dumpHistory (RunHistory pkgSet0 historySeq) = do
    say "Updater's past history:"
    say $ unwords
        [ "Initial state:"
        , show $ printPkg <$> Set.toList (getPkgs pkgSet0)
        ]
    CM.forM_ historyList $ \(n, entry, ec) -> say $ unwords
        [ "Pass"
        , show n ++ ":"
        , show $ printPkg <$> Set.toList (getPkgs entry)
        , show ec
        ]
    say ""
  where historyList :: [(Int, PendingPackages, ExitArg m)]
        historyList =
            [ (n, entry, ec)
            | ((entry, ec), n) <- zip (toList historySeq) [1..]
            ]

-- | An action that controls the looping mechanism inside 'runUpdater'. This
--   holds the logic for what action to take after running @emerge@ (e.g. exit
--   with success/error or continue to the next iteration).
type UpdaterLoop m
    =  m () -- ^ The next iteration of the loop
    -> m ()

-- | State between each run of an 'UpdaterLoop'
type UpdateState m =
    ( BuildPkgs -- ^ Current targets and other info needed to run the PM
    , RunHistory m
    )

-- | The default run environment with 'IO' at its base.
newtype IOEnv a
    = IOEnv (StateT (UpdateState IOEnv) (EnvT IO) a)
    deriving ( Functor, Applicative, Monad, MonadSay, MonadPkgState
             , MonadWritePkgState, MonadState (UpdateState IOEnv)
             , HasPkgManager, HasRawPMArgs )

instance MonadExit IOEnv where
    type ExitArg IOEnv = ExitArg (StateT (UpdateState IOEnv) (EnvT IO))
    success = IOEnv . success
    die = IOEnv . die
    exitWith = IOEnv . exitWith
    isSuccess (_ :: Proxy IOEnv)
        = isSuccess (Proxy :: Proxy (StateT (UpdateState IOEnv) (EnvT IO)))

runIOEnv :: IOEnv a -> UpdateState IOEnv -> EnvT IO a
runIOEnv (IOEnv s) = evalStateT s

-- | Run the main part of @haskell-updater@ (e.g. not @--help@,
--   @--version@, or list mode). This expects the initial 'UpdateState' to
--   reflect the initial state of the system.
runUpdater
    :: forall m.
        ( MonadSay m
        , MonadPkgState m
        , MonadWritePkgState m
        , MonadState (UpdateState m) m
        , MonadExit m
        , Show (ExitArg m)
        , HasPkgManager m
        , HasRawPMArgs m
        )
    => m ()
runUpdater = do
    pkgMgr <- askPkgManager
    (bps, _) <- get
    case getLoopType pkgMgr of
        UntilNoPending -> runLoop loopUntilNoPending
        UntilNoChange -> runLoop loopUntilNoChange
        NoLoop -> buildPkgs bps >>= exitWith
    success "done!"
  where
    -- | Only abort the loop when there are no broken packages reported
    --   on the system, or if a loop is detected by comparing the state to
    --   previous runs.
    loopUntilNoPending :: UpdaterLoop m
    loopUntilNoPending continue = do
        (bps, hist) <- get

        let ps = buildPkgsPending bps

            -- Stop when there are no more pending packages
        if  | Set.null (getPkgs ps) -> alertDone Nothing

            -- Look to see if the current set of broken haskell packages matches
            -- any in the history. If it does, this means we're in a loop.
            -- (Ignore this if the first run has not been completed yet.)
            | not (isEmptyHistory hist) && isInHistory hist (getPkgs ps)
                -> alertStuck Nothing

            -- Otherwise, keep going
            | otherwise -> continue

    -- | Compare the /last two/ runs to see if any broken packages were fixed.
    --   If the state of broken packages stays the same between runs, it means
    --   emerge is either hitting an error that @haskell-updater@ cannot fix,
    --   or emerge has nothing to do.
    loopUntilNoChange :: UpdaterLoop m
    loopUntilNoChange continue = do
        (bps, hist) <- get

        if noTargets bps
            then done
            else case historyState hist of
                -- If there is no history, the first update still needs to be run
                NoRunsTried _ -> continue

                OneRunTried initialPending (lastPending, lastEC)
                    -> go initialPending lastPending lastEC True

                MultipleRunsTried (earlierPending, _) (lastPending, lastEC)
                    -> go earlierPending lastPending lastEC False

      where
        go :: PendingPackages -> PendingPackages -> ExitArg m -> Bool -> m ()
        go earlierPending lastPending lastEC isFirstRun = do
            let nothingChanged, cmdSuccess :: Bool

                -- Is the broken package set identical to what it was
                -- before the update?
                nothingChanged = lastPending == earlierPending

                -- Did the update command succeed?
                cmdSuccess = isSuccess (Proxy :: Proxy m) lastEC

                -- Alert that we're stuck, but add a note if it failed on its
                -- first run
                stuck = alertStuck $ if isFirstRun
                    then Just stuckOnePass
                    else Nothing

            case (nothingChanged, cmdSuccess) of
                -- The success state: The update completed and nothing changed
                (True, True) -> done

                -- Stuck state: The update failed and nothing changed
                (True, False) -> stuck

                -- A change happened and we have targets still: continue
                (False, _) -> continue

        -- Alert that we're finished, but add a warning if there are
        -- still broken packages on the system
        done = do
            (_, hist) <- get
            let lastPending = latestPending hist
            alertDone $ if Set.null (getPkgs lastPending)
                then Nothing
                else Just successIncomplete

        -- Are there no targets for the package manager?
        noTargets = emptyTargets . buildPkgsTargets

        -- This mostly comes up with the 'UntilNoChange' loop type, so we'll limit
        -- it to that for now.
        stuckOnePass =
            [ "NOTE: The updater loop failed after one pass, with no changes to the state of"
            , "broken Haskell packages on the system. This is often caused by the package"
            , "manager failing during dependency resolution. Check the output to be sure."
            ]

        successIncomplete =
            [ "WARNING: The updater loop appears to have completed, but there are still"
            , "broken Haskell packages detected on the system!"
            ]

    -- Rebuild the packages then retrieve fresh package state
    updateAndContinue :: UpdaterLoop m -> m ()
    updateAndContinue loop = do
        (bps, hist) <- get

        -- Exit with failure if there are no targets for the package manager
        when (emptyTargets (buildPkgsTargets bps)) alertNoTargets

        exitCode <- buildPkgs bps

        bps' <- getPackageState
        let ps = buildPkgsPending bps'
            hist' = hist
                { runHistory = runHistory hist |> (ps, exitCode) }

        put (bps', hist')

        runLoop loop

    runLoop :: UpdaterLoop m -> m ()
    runLoop loop = loop (updateAndContinue loop)

    emptyTargets :: Set.Set Types.Target -> Bool
    emptyTargets = all $ \case
        TargetInvalid ps -> Set.null (getPkgs ps)
        TargetAll ps -> Set.null (getPkgs ps)
        CustomTarget _ -> False

    alertDone maybeMsg = success $ unlines
        $ "Nothing to build!"
        : maybe [] ("":) maybeMsg

    alertStuck maybeMsg = do
        (_,hist) <- get
        dumpHistory hist
        die $ unlines
            $ "Updater stuck in the loop and can't progress"
            : maybe [] ("":) maybeMsg

    alertNoTargets = die "No targets to pass to the package manager!"

-- | As needed, query @ghc-pkg check@ for broken packages, scan the filesystem
--   for installed packages, and look for misc breakages. Return the results
--   summarized for use with 'buildPkgs'.
getPackageState
    :: (MonadSay m, MonadPkgState m, HasPkgManager m)
    => m BuildPkgs
getPackageState = askPkgManager >>= \pkgMgr ->
    case runMode pkgMgr of
        Left mode -> fromRunMode mode
        Right (PortageBasicMode (Left PreservedRebuild)) -> do
            ps <- InvalidPending <$> getInvalid
            let ct = CustomTarget "@preserved-rebuild"
            pure $ BuildNormal pkgMgr ps (Set.singleton ct)
        Right (PortageBasicMode (Right targ)) -> fromRunMode (BasicMode targ)
        Right (PortageListMode targ) -> fromRunMode (ListMode targ)
        Right (ReinstallAtomsMode targ) -> flip evalStateT Nothing $ do
            aps <- getAll
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
                    p <- InvalidPending <$> getInvalid
                    pure (p, extraTargs th)
            pure $ BuildRAMode p ts aps
  where
    fromRunMode
        :: (MonadSay m, MonadPkgState m, HasPkgManager m)
        => RunMode
        -> m BuildPkgs
    fromRunMode mode = askPkgManager >>= \pkgMgr -> do
        ps <- case getTarget mode of
            OnlyInvalid -> InvalidPending <$> getInvalid
            AllInstalled -> AllPending <$> getAll
        pure $ BuildNormal pkgMgr ps Set.empty

    getInvalid :: (MonadSay m, MonadPkgState m) => m InvalidPkgs
    getInvalid = do
        say "Searching for packages installed with a different version of GHC."
        say ""
        old <- oldGhcPkgs
        pkgListPrintLn "old" old

        say "Searching for Haskell libraries with broken dependencies."
        say ""
        (broken, unknown_packages, unknown_files) <- brokenPkgs
        let broken' = Set.fromList broken
        printUnknownPackagesLn (map unCPV unknown_packages)
        printUnknownFilesLn unknown_files
        pkgListPrintLn "broken" (notGHC broken')

        return $ InvalidPkgs $ old <> broken'

    getAll :: (MonadSay m, MonadPkgState m) => m AllPkgs
    getAll = do
        say "Searching for packages installed with the current version of GHC."
        say ""
        pkgs <- allInstalledPkgs
        pkgListPrintLn "installed" pkgs
        return $ AllPkgs pkgs

    printUnknownPackagesLn [] = return ()
    printUnknownPackagesLn ps = do
        say "The following packages are orphan (not installed by your package manager):"
        printList id ps
        say ""
    printUnknownFilesLn [] = return ()
    printUnknownFilesLn fs = do
        say "The following files are orphan (not installed by your package manager):"
        printList id fs
        say "It is strongly advised to remove orphans:"
        say "    One of known sources of orphans is packages installed before 01 Jan 2015."
        say "    If you know it's your case you can easily remove such files:"
        say "        # rm -v -- `qfile -o $(ghc --print-libdir)/package.conf.d/*.conf $(ghc --print-libdir)/gentoo/*.conf`"
        say "        # ghc-pkg recache"
        say "    It will likely need one more 'haskell-updater' run."
        say ""

    withIPCache
        :: (MonadSay m, MonadPkgState m)
        => (InvalidPkgs -> a)
        -> StateT (Maybe InvalidPkgs) m a
    withIPCache f = fmap f $ get >>= \case
        Nothing -> do
            ips <- getInvalid
            put (Just ips)
            pure ips
        Just ips -> pure ips

-- -----------------------------------------------------------------------------
-- Printing information.

help :: IO a
help = progInfo >>= sayIO . success

version :: IO a
version = fmap (++ '-' : showVersion Paths.version) getProgName >>= sayIO . success

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
    :: (MonadSay m, MonadIO m)
    => PkgManager
    -> RawPMArgs
    -> m ()
systemInfo pkgMgr rawArgs = do
    ver    <- liftIO ghcVersion
    pName  <- liftIO getProgName
    let pVer = showVersion Paths.version
    pLoc   <- liftIO ghcLoc
    libDir <- liftIO ghcLibDir
    say $ "Running " ++ pName ++ "-" ++ pVer ++ " using GHC " ++ ver
    say $ "  * Executable: " ++ pLoc
    say $ "  * Library directory: " ++ libDir
    say $ "  * Package manager (PM): " ++ nameOfPM (toPkgManager pkgMgr)
    unless (null rawArgs) $
        say $ "  * PM auxiliary arguments: " ++ unwords rawArgs
    say $ "  * Targets: " ++ L.intercalate ", " ts
    say $ "  * Mode: " ++ argString m
    say ""
  where
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
