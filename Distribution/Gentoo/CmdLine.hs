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

parseArgs :: PkgManager -> RawPMArgs -> Either String (CmdLineArgs, RawPMArgs)
parseArgs defPM args = case getOpt' Permute options args of
    (_, _, _, errs@(_:_)) -> Left $ unwords $ "Errors in arguments:" : errs
    (_, _, unk@(_:_), _) -> Left $ unwords $ "Unknown options:" : unk
    (fs, raw, _, _) ->
        (,raw) <$> foldr (>=>) pure fs (defCmdLineArgs defPM)

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

    mkMode :: RunMode -> Either String Mode.RunMode
    mkMode = \case
        BasicMode -> Mode.BasicMode <$> mkTarget (cmdLineTarget cmdLine)
        ListMode -> Mode.ListMode <$> mkTarget (cmdLineTarget cmdLine)
        ReinstallAtomsMode -> Left
            "reinstall-atoms mode is only supported by the portage package manager"

    mkPortageMode
        :: RunMode
        -> Either String (Either Mode.RunMode Mode.ReinstallAtomsMode)
    mkPortageMode = \case
        BasicMode -> Left . Mode.BasicMode <$> mkTarget (cmdLineTarget cmdLine)
        ListMode -> Left . Mode.ListMode <$> mkTarget (cmdLineTarget cmdLine)
        ReinstallAtomsMode -> Right . Mode.ReinstallAtomsMode
            <$> mkRATarget (cmdLineTarget cmdLine)

    mkTarget :: Either CustomTargets BuildTarget -> Either String Mode.Target
    mkTarget = \case
        Right OnlyInvalid -> Right Mode.OnlyInvalid
        Right AllInstalled -> Right Mode.AllInstalled
        Right WorldTarget -> Left
            "world target is only supported in reinstall-atoms mode"
        Left _ -> Left
            "custom targets are only supported in reinstall-atoms mode"

    mkRATarget
        :: Either CustomTargets BuildTarget
        -> Either String (Either Mode.Target Mode.ReinstallAtomsTarget)
    mkRATarget = Right . \case
        Right OnlyInvalid -> Left Mode.OnlyInvalid
        Right AllInstalled -> Left Mode.AllInstalled
        Right WorldTarget -> Right Mode.WorldTarget
        Left cts -> Right $ Mode.CustomTargets cts

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
