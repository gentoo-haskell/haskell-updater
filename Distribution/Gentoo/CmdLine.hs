{- |
   Module      : Distribution.Gentoo.CmdLine.Types

   Functions and logic for parsing command line options and converting them
   into a valid internal representation of haskell-updater modes (see
   @Distribution.Gentoo.Types.HUMode@).
 -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.Gentoo.CmdLine
  ( parseArgs
  , mkHUMode
  , options
  , argString
  ) where

import           Control.Monad         ((>=>))
import           Data.Proxy
import           System.Console.GetOpt

import Distribution.Gentoo.CmdLine.Types
import Distribution.Gentoo.PkgManager
import Distribution.Gentoo.PkgManager.Types
import Distribution.Gentoo.Types
import qualified Distribution.Gentoo.Types.HUMode as Mode
import Output

-- | Process arguments from the command line. Returns an error string if the
--   user provided incorrect or unknown options.
parseArgs :: PkgManager -> RawPMArgs -> Either String (CmdLineArgs, RawPMArgs)
parseArgs defPM args = case getOpt' Permute options args of
    (_, _, _, errs@(_:_)) -> Left $ unwords $ "Errors in arguments:" : errs
    (_, _, unk@(_:_), _) -> Left $ unwords $ "Unknown options:" : unk
    (fs, raw, _, _) ->
        (,raw) <$> foldr (>=>) pure fs (defCmdLineArgs defPM)

-- | Parse processed command line arguments into a 'Mode.HUMode'. Returns an
--   error string if the user supplied non-compatible option combinations.
mkHUMode :: CmdLineArgs -> RawPMArgs -> Either String Mode.HUMode
mkHUMode cmdLine raw
    | cmdLineHelp cmdLine = pure Mode.HelpMode
    | cmdLineVersion cmdLine = pure Mode.VersionMode
    | otherwise = do
        mPkgMgr <- mkPkgManager (cmdLinePkgManager cmdLine)
        pure $ Mode.RunMode runModifier mPkgMgr
  where
    mkPkgManager :: PkgManager -> Either String Mode.PkgManager
    mkPkgManager = \case
        Portage -> Mode.Portage <$> mkPortageMode (cmdLineMode cmdLine)
        PkgCore -> Mode.PkgCore <$> mkMode (cmdLineMode cmdLine)
        Paludis -> Mode.Paludis <$> mkMode (cmdLineMode cmdLine)
        CustomPM pm -> Mode.CustomPM pm <$> mkMode (cmdLineMode cmdLine)
        pm@(InvalidPM _) -> Left $
            "Invalid package manager in mkHUMode: " ++ show pm

    -- Logic for parsing modes for non-portage package managers
    mkMode :: RunMode -> Either String Mode.RunMode
    mkMode = \case
        BasicMode -> Mode.BasicMode <$> mkTarget (cmdLineTarget cmdLine)
        ListMode -> Mode.ListMode <$> mkTarget (cmdLineTarget cmdLine)
        ReinstallAtomsMode -> Left
            "reinstall-atoms mode is only supported by the portage package manager"

    -- Logic for parsing targets for non-portage package managers
    mkTarget :: Either CustomTargets BuildTarget -> Either String Mode.Target
    mkTarget = \case
        Right OnlyInvalid -> Right Mode.OnlyInvalid
        Right AllInstalled -> Right Mode.AllInstalled
        Right PreservedRebuild -> Left $
            "preserved-rebuild target is only supported by the portage \
            \package manager"
        Right WorldTarget -> Left
            "world target is only supported in reinstall-atoms mode"
        Left _ -> Left
            "custom targets are only supported in reinstall-atoms mode"

    -- Logic for parsing modes for portage
    mkPortageMode
        :: RunMode
        -> Either String Mode.PortageMode
    mkPortageMode = \case
        BasicMode -> Mode.PortageBasicMode
            <$> mkPortageBasicTarget (cmdLineTarget cmdLine)
        ListMode -> Mode.PortageListMode
            <$> mkPortageTarget (cmdLineTarget cmdLine)
        ReinstallAtomsMode -> Mode.ReinstallAtomsMode
            <$> mkPortageRATarget (cmdLineTarget cmdLine)

    -- Logic for parsing targets for portage's basic mode
    mkPortageBasicTarget
        :: Either CustomTargets BuildTarget
        -> Either String (Either Mode.PortageBasicTarget Mode.Target)
    mkPortageBasicTarget = \case
        Right PreservedRebuild -> Right $ Left Mode.PreservedRebuild
        targ -> Right <$> mkPortageTarget targ

    -- Logic for parsing targets for portage's reinstall-atoms mode
    mkPortageRATarget
        :: Either CustomTargets BuildTarget
        -> Either String (Either Mode.Target Mode.ReinstallAtomsTarget)
    mkPortageRATarget = \case
        Right WorldTarget -> Right $ Right $
            if cmdLineWorldFull cmdLine
                then Mode.WorldFullTarget
                else Mode.WorldTarget
        Left cts -> Right $ Right $ Mode.CustomTargets cts
        targ -> Left <$> mkPortageTarget targ

    -- Logic for parsing targets for portage's list mode; also common logic
    -- for parsing targets, between portage's basic and reinstall-atoms modes
    mkPortageTarget
        :: Either CustomTargets BuildTarget
        -> Either String Mode.Target
    mkPortageTarget = \case
        Right OnlyInvalid -> Right Mode.OnlyInvalid
        Right AllInstalled -> Right Mode.AllInstalled
        Right PreservedRebuild -> Left
            "preserved-rebuild target is only supported in basic mode"
        Right WorldTarget -> Left
            "world target is only supported in reinstall-atoms mode"
        Left _ -> Left
            "custom targets are only supported in reinstall-atoms mode"

    runModifier :: RunModifier
    runModifier = RM
        { flags = (if cmdLinePretend cmdLine then (PretendBuild:) else id)
                    $ if cmdLineNoDeep cmdLine
                        then [UpdateAsNeeded]
                        else [UpdateDeep]
        , withCmd = cmdLineAction cmdLine
        , rawPMArgs = raw
        , verbosity = cmdLineVerbosity cmdLine
        }

options :: [OptDescr (CmdLineArgs -> Either String CmdLineArgs)]
options =
    [ Option ['P'] ["package-manager"]
      (ReqArg mkPM "PM")
        $ "Use package manager PM, where PM can be one of:\n"
              ++ pmList ++ defPM
    , Option ['C'] ["custom-pm"]
      (ReqArg (\s c -> pure $ c { cmdLinePkgManager = CustomPM s }) "command")
        $ "Use custom command as package manager;\n"
          ++ "    ignores the --pretend and --no-deep flags."
    , Option ['p'] ["pretend"]
        (naUpdate $ \c -> c { cmdLinePretend = True } )
        "Only pretend to build packages."
    , Option []    ["no-deep"]
        (naUpdate $ \c -> c { cmdLineNoDeep = True } )
        "Don't pull deep dependencies (--deep with emerge)."
    , Option ['V'] ["version"]
        (naUpdate $ \c -> c { cmdLineVersion = True })
        "Version information."
    , Option []    ["action"]
        (ReqArg (fromCmdline (\a c -> c { cmdLineAction = a })) "action")
        (argHelp (Proxy @WithCmd))
    , Option []    ["target"]
        (ReqArg (fromCmdline (\a -> updateTarget (Right a))) "target")
        (argHelp (Proxy @BuildTarget))
    , Option ['c'] ["dep-check"]
        (naUpdate $ updateTarget (Right OnlyInvalid))
        $ "alias for --target=" ++ argString OnlyInvalid
      -- deprecated alias for 'dep-check'
    , Option ['u'] ["upgrade"]
        (naUpdate $ updateTarget (Right OnlyInvalid))
        $ "alias for --target=" ++ argString OnlyInvalid
    , Option ['a'] ["all"]
        (naUpdate $ updateTarget (Right AllInstalled))
        $ "alias for --target=" ++ argString AllInstalled
    , Option ['W']    ["world"]
        (naUpdate $ \c -> updateTarget (Right WorldTarget) c
            { cmdLinePkgManager = Portage
            , cmdLineMode = ReinstallAtomsMode
            }
        ) $      "alias for --package-manager=portage"
         ++ " \\\n          --target=" ++ argString WorldTarget
         ++ " \\\n          --mode=" ++ argString ReinstallAtomsMode
    , Option [] ["world-full"]
        (naUpdate $ \c -> updateTarget (Right WorldTarget) c
            { cmdLinePkgManager = Portage
            , cmdLineMode = ReinstallAtomsMode
            , cmdLineWorldFull = True
            }
        ) $ "alias for --world -- --newuse --with-bdeps=y"
    , Option [] ["preserved-rebuild"]
        (naUpdate $ updateTarget (Right PreservedRebuild))
        $ "alias for --target=" ++ argString PreservedRebuild
    , Option ['T'] ["custom-target"]
        (ReqArg
            (\s c -> pure $ updateTarget (Left s) c
                { cmdLinePkgManager = Portage
                , cmdLineMode = ReinstallAtomsMode
                }
            )
        "target")
        $  "Use a custom target. May be given multiple times.\n"
        ++ "    Enables portage PM and reinstall-targets mode.\n"
        ++ "    Will override any non-custom targets."
    , Option []    ["mode"]
        (ReqArg (fromCmdline (\a c -> c { cmdLineMode = a })) "mode")
        (argHelp (Proxy @RunMode))
    , Option ['l'] ["list-only"]
        (naUpdate $ \c -> c { cmdLineMode = ListMode })
        $ "alias for --mode=" ++ argString ListMode
    , Option ['R']    ["reinstall-atoms"]
        (naUpdate $ \c -> c { cmdLineMode = ReinstallAtomsMode })
        $ "alias for --mode=" ++ argString ReinstallAtomsMode
    , Option ['q']      ["quiet"]
        (naUpdate $ \c -> c { cmdLineVerbosity = Quiet })
        "Print only fatal errors (to stderr)."
    , Option ['v']      ["verbose"]
        (naUpdate $ \c -> c { cmdLineVerbosity = Verbose })
        "Be more elaborate (to stderr)."
    , Option ['h', '?'] ["help"]
        (naUpdate $ \c -> c { cmdLineHelp = True })
        "Print this help message."
    ]

  where
    naUpdate f = NoArg (pure . f)

    -- This touches some legacy code so we need a custom handler for it
    mkPM :: String -> CmdLineArgs -> Either String CmdLineArgs
    mkPM s c = case choosePM s of
        InvalidPM pm -> Left $ "Unknown package manager: " ++ pm
        Portage -> Right $ c { cmdLinePkgManager = Portage }
        Paludis -> Right $ c { cmdLinePkgManager = Paludis }
        PkgCore -> Right $ c { cmdLinePkgManager = PkgCore }
        CustomPM _ -> error "Undefined behavior in mkPM"

    pmList = unlines . map (" * " ++) $ definedPMs
    defPM = "The last valid value of PM specified is chosen.\n\
            \    The default package manager is: " ++ defaultPMName ++ ",\n\
            \    which can be overriden with the \"PACKAGE_MANAGER\"\n\
            \    environment variable."

    -- Custom targets always override BuildTargets
    -- New custom targets are appended to old custom targets
    -- New BuildTargets override old BuildTargets
    updateTarget :: Either String BuildTarget -> CmdLineArgs -> CmdLineArgs
    updateTarget new old =
        let newT = case (new, cmdLineTarget old) of
                            -- Override old BuildTargets with new BuildTargets
                            (Right t, Right _) -> Right t
                            -- Append new custom target
                            (Left s, Left ss) -> Left $ ss ++ [s]
                            -- Drop old BuildTargets for new custom target
                            (Left s, Right _) -> Left [s]
                            -- Drop new BuildTargets in favor of old custom targets
                            (Right _, Left ss) -> Left ss
        in old { cmdLineTarget = newT }
