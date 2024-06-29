
module Distribution.Gentoo.CmdLine.Types where

import Data.Proxy

import Distribution.Gentoo.PkgManager.Types
import Distribution.Gentoo.Types
import Output

data CmdLineArgs = CmdLineArgs
    { cmdLinePkgManager :: PkgManager
    , cmdLineCustomPM :: Maybe String
    , cmdLinePretend :: Bool
    , cmdLineNoDeep :: Bool
    , cmdLineVersion :: Bool
    , cmdLineAction :: WithCmd
    , cmdLineTarget :: BuildTarget
    , cmdLineMode :: RunMode
    , cmdLineVerbosity :: Verbosity
    , cmdLineHelp :: Bool
    } deriving (Show, Eq, Ord)

defCmdLineArgs :: PkgManager -> CmdLineArgs
defCmdLineArgs defPM = CmdLineArgs
    defPM
    Nothing
    False
    False
    False
    PrintAndRun
    OnlyInvalid
    BasicMode
    Normal
    False

data BuildTarget
    = OnlyInvalid -- ^ Default
    | AllInstalled -- ^ Rebuild every haskell package
    | WorldTarget -- ^ Target @world portage set
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

data RunMode
    = BasicMode
    | ListMode
    | ReinstallAtomsMode
    deriving (Show, Eq, Ord, Enum, Bounded)

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

instance CmdlineOpt BuildTarget where
    argInfo OnlyInvalid = ("invalid", Just "broken Haskell packages")
    argInfo AllInstalled = ("all", Just "all installed Haskell packages")
    argInfo WorldTarget =
        ( "world"
        , Just $ "@world set (only valid with portage package\n"
              ++ "manager and reinstall-atoms mode)"
        )

    optName _ = "target"
    optDescription _ =
        "Choose the type of packages for the PM to target"
    optDefault _ = OnlyInvalid

instance CmdlineOpt RunMode where
    argInfo BasicMode = ("basic", Just "classic haskell-updater behavior")
    argInfo ListMode =
        ( "list"
        , Just $ "just print a list of packages for rebuild,\n"
              ++ "one package per line"
        )
    argInfo ReinstallAtomsMode =
        ( "reinstall-atoms"
        , Just $ "experimental portage invocation using\n"
              ++ "--reinstall-atoms (may be more useful in\n"
              ++ "some situations)" )

    optName _ = "mode"
    optDescription _ =
        "Mode of operation for haskell-updater"
    optDefault _ = BasicMode
