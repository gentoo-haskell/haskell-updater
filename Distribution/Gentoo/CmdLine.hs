{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.Gentoo.CmdLine
  ( parseArgs
  , options
  ) where

import           Control.Monad         ((>=>))
import           Data.Char             (toLower)
import qualified Data.List             as L
import           Data.Proxy
import           System.Console.GetOpt

import Distribution.Gentoo.PkgManager
import Distribution.Gentoo.PkgManager.Types
import Distribution.Gentoo.Types
import Output

-- -----------------------------------------------------------------------------
-- Command-line flags

-- | A class for multiple-choice options selected by an argument on the command
--   line
class (Eq a, Enum a, Bounded a) => CmdlineOpt a where
    -- | Define the short name and an optional description for a constructor
    argInfo :: a -> (String, Maybe String)
    -- | Define the short name for the multiple-choice argument as a whole
    --
    --   e.g. @"action"@
    optName :: Proxy a -> String
    -- | Define the description for the multiple-choice argument as a whole
    --
    --   e.g. @"Specify whether to run the PM command or just print it"@
    optDescription :: Proxy a -> String
    -- | Define the default constructor for the multiple-choice argument
    --
    --   e.g. 'PrintAndRun'
    optDefault :: Proxy a -> a

instance CmdlineOpt WithCmd where
    argInfo PrintAndRun = ("print-and-run", Nothing)
    argInfo PrintOnly = ("print", Nothing)
    argInfo RunOnly = ("run", Nothing)

    optName _ = "action"

    optDescription _ =
        "Specify whether to run the PM command or just print it"

    optDefault _ = PrintAndRun

argString :: CmdlineOpt a => a -> String
argString = fst . argInfo

argDescription :: CmdlineOpt a => a -> Maybe String
argDescription = snd . argInfo

argHelp :: forall a. CmdlineOpt a => Proxy a -> String
argHelp _ = unlines $ [mainDesc] ++ (args >>= argLine)
  where
    mainDesc = optDescription (Proxy @a)
    argLine a = case (L.lookup a argFields, argDescription a) of
        (Nothing, _) -> []
        (Just s, Nothing) ->  [s]
        (Just s, Just d) -> [s ++ padding s ++ " : " ++ d]
    padding s =
        let mx = maximum $ length . snd <$> argFields
        in replicate (mx - length s) ' '
    argFields = (\a -> (a, showArg a)) <$> args
    showArg a = " * " ++ argString a ++ showDef a
    showDef a
        | optDefault (Proxy @a) == a = " (default)"
        | otherwise = ""
    args = [minBound :: a .. maxBound]

fromCmdline
    :: forall a. CmdlineOpt a
    => (a -> RunModifier -> RunModifier)
    -> String
    -> RunModifier
    -> Either String RunModifier
fromCmdline update s rm =
    case L.find (\a -> argString a == lowerS) args of
        Nothing -> Left $ "Unknown " ++ name ++ ": " ++ lowerS
        Just a -> Right $ update a rm
  where
    lowerS = map toLower s
    name = optName $ Proxy @a
    args = [minBound :: a .. maxBound]

parseArgs :: PkgManager -> [String] -> Either String RunModifier
parseArgs defPM args = case getOpt' Permute options args of
    (_, _, _, errs@(_:_)) -> Left $ unwords $ "Errors in arguments:" : errs
    (_, _, unk@(_:_), _) -> Left $ unwords $ "Unknown options:" : unk
    (fs, raw, _, _) ->
        postProcessRM <$> foldr (>=>) pure fs (defRunModifier defPM raw)

defRunModifier :: PkgManager -> [String] -> RunModifier
defRunModifier defPM raw = RM
    { pkgmgr = defPM
    , flags = []
    , withCmd = optDefault $ Proxy @WithCmd
    , rawPMArgs = raw
    , verbosity = Normal
    , listOnly = False
    , showHelp = False
    , showVer = False
    , target = OnlyInvalid
    }

-- | Make sure there is at least one of 'UpdateAsNeeded' or 'UpdateDeep'
--   in 'flags'.
postProcessRM :: RunModifier -> RunModifier
postProcessRM rm = rm { flags = flags' }
  where
    flags'
        | or $ [(==UpdateAsNeeded), (==UpdateDeep)] <*> nubFlags = nubFlags
        | otherwise = UpdateDeep : nubFlags
    nubFlags = L.nub (flags rm)

options :: [OptDescr (RunModifier -> Either String RunModifier)]
options =
    [ Option ['c'] ["dep-check"]
        (naUpdate $ \r -> r { target = OnlyInvalid })
        "Check dependencies of Haskell packages."
      -- deprecated alias for 'dep-check'
    , Option ['u'] ["upgrade"]
        (naUpdate $ \r -> r { target = OnlyInvalid })
        "Rebuild Haskell packages after a GHC upgrade."
    , Option ['a'] ["all"]
        (naUpdate $ \r -> r { target = AllInstalled })
        "Rebuild all Haskell libraries built with current GHC."
    , Option ['P'] ["package-manager"]
      (ReqArg mkPM "PM")
        $ "Use package manager PM, where PM can be one of:\n"
              ++ pmList ++ defPM
    , Option ['C'] ["custom-pm"]
      (ReqArg (\s r -> pure $ r { pkgmgr = CustomPM s }) "command")
        $ "Use custom command as package manager;\n"
          ++ "ignores the --pretend and --no-deep flags."
    , Option ['p'] ["pretend"]
        (naUpdate $ \r -> r { flags = PretendBuild : flags r } )
        "Only pretend to build packages."
    , Option []    ["no-deep"]
        (naUpdate $ \r -> r { flags = UpdateAsNeeded : flags r } )
        "Don't pull deep dependencies (--deep with emerge)."
    , Option ['l'] ["list-only"]
        (naUpdate $ \r -> r { listOnly = True })
        "Output only list of packages for rebuild. One package per line."
    , Option ['V'] ["version"]
        (naUpdate $ \r -> r { showVer = True })
        "Version information."
    , Option []    ["action"]
        (ReqArg (fromCmdline (\a r -> r { withCmd = a })) "action")
        (argHelp (Proxy @WithCmd))
    , Option ['q']      ["quiet"]
        (naUpdate $ \r -> r { verbosity = Quiet })
        "Print only fatal errors (to stderr)."
    , Option ['v']      ["verbose"]
        (naUpdate $ \r -> r { verbosity = Verbose })
        "Be more elaborate (to stderr)."
    , Option ['h', '?'] ["help"]
        (naUpdate $ \r -> r { showHelp = True })
        "Print this help message."
    ]
  where
    naUpdate f = NoArg (pure . f)

    -- This touches some legacy code so we need a custom handler for it
    mkPM :: String -> RunModifier -> Either String RunModifier
    mkPM s rm = case choosePM s of
        InvalidPM pm -> Left $ "Unknown package manager: " ++ pm
        Portage -> Right $ rm { pkgmgr = Portage }
        Paludis -> Right $ rm { pkgmgr = Paludis }
        PkgCore -> Right $ rm { pkgmgr = PkgCore }
        CustomPM _ -> undefined

    pmList = unlines . map (" * " ++) $ definedPMs
    defPM = "The last valid value of PM specified is chosen.\n\
            \The default package manager is: " ++ defaultPMName ++ ",\n\
            \which can be overriden with the \"PACKAGE_MANAGER\"\n\
            \environment variable."
