{- |
   Module      : Distribution.Gentoo.CmdLine.Types

   Functions and logic for parsing command line options and converting them
   into a valid internal representation of haskell-updater modes (see
   @Distribution.Gentoo.Types.Mode@).
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
import qualified Data.List.NonEmpty as NE
import           Data.Monoid (Ap(..))
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Semigroup (Last(..), sconcat)
import           System.Console.GetOpt

import Distribution.Gentoo.CmdLine.Types
import Distribution.Gentoo.PkgManager
import Distribution.Gentoo.PkgManager.Types
import Distribution.Gentoo.Types
import qualified Distribution.Gentoo.Types.Mode as Mode
import Distribution.Gentoo.Util (These(..), singletonNE)
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
        BasicMode -> Mode.BasicMode <$> go (cmdLineTargets cmdLine)
        ListMode -> Mode.ListMode <$> go (cmdLineTargets cmdLine)
        ReinstallAtomsMode -> Left
            "reinstall-atoms mode is only supported by the portage package manager"
      where
        go = maybe (Right Mode.OnlyInvalid) (onlyLast mkTarget)

    -- Logic for parsing targets for non-portage package managers
    mkTarget :: Either CustomTarget BuildTarget -> Either String Mode.Target
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
            <$> withDefTarget
                    (onlyLast mkPortageBasicTarget)
                    maybeTargs
        ListMode -> Mode.PortageListMode
            <$> withDefTarget
                    (onlyLast mkPortageTarget)
                    maybeTargs
        ReinstallAtomsMode -> Mode.ReinstallAtomsMode
            <$> withDefTarget mkPortageRATarget maybeTargs
      where
        maybeTargs = cmdLineTargets cmdLine

    -- Logic for parsing targets for portage's basic mode
    mkPortageBasicTarget
        :: Either CustomTarget BuildTarget
        -> Either String (Either Mode.PortageBasicTarget Mode.Target)
    mkPortageBasicTarget = \case
        Right PreservedRebuild -> Right $ Left Mode.PreservedRebuild
        targ -> Right <$> mkPortageTarget targ

    -- Logic for parsing targets for portage's reinstall-atoms mode
    mkPortageRATarget
        :: NE.NonEmpty (Either CustomTarget BuildTarget)
        -> Either String Mode.RATargets
    mkPortageRATarget = foldTargets $ \case
        Left ct -> Right $ That $ That $ singletonNE ct
        Right WorldTarget -> Right $ That $ This $
            if cmdLineWorldFull cmdLine
                then Mode.WorldFullTarget
                else Mode.WorldTarget
        targ -> This <$> mkPortageTarget targ

    -- Logic for parsing targets for portage's list mode; also common logic
    -- for parsing targets, between portage's basic and reinstall-atoms modes
    mkPortageTarget
        :: Either CustomTarget BuildTarget
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

    -- Uses the default target if none were specified on the command line
    withDefTarget
        :: (NE.NonEmpty (Either CustomTarget BuildTarget) -> a)
        -> Maybe (NE.NonEmpty (Either CustomTarget BuildTarget))
        -> a
    withDefTarget f = f . fromMaybe (NE.singleton defTarget)
      where
        defTarget = Right OnlyInvalid

    -- Uses 'Data.Semigroup.Last' to only grab the last target specified
    onlyLast
        :: Applicative f
        => (a -> f b)
        -> NE.NonEmpty a
        -> f b
    onlyLast f = fmap (getLast . sconcat . fmap Last) . traverse f

    -- Uses 'Data.Monoid.Ap' to combine RATargets inside an Applicative
    foldTargets
        :: (Applicative f, Semigroup b)
        => (a -> f b)
        -> NE.NonEmpty a
        -> f b
    foldTargets f = getAp . sconcat . fmap (Ap . f)

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
        "Use a custom target. May be given multiple times.\n\
        \    Enables portage PM and reinstall-targets mode."
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

    updateTarget :: Either CustomTarget BuildTarget -> CmdLineArgs -> CmdLineArgs
    updateTarget new old =
        let ne = NE.singleton new
            newT = maybe ne (<> ne) (cmdLineTargets old)
        in old { cmdLineTargets = Just newT }
