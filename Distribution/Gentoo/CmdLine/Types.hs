{- |
   Module      : Distribution.Gentoo.CmdLine.Types

   Types representing command-line options
 -}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.Gentoo.CmdLine.Types
    (
      -- * main type
      CmdLineArgs(..)
    , defCmdLineArgs
      -- * sub types
    , BuildTarget(..)
    , RunMode(..)
    , CmdlineOpt(..)
      -- * getter functions
    , argString
    , argDescription
      -- * utility functions
    , argHelp
    , fromCmdline
    ) where

import           Data.Char             (toLower)
import qualified Data.List             as L
import           Data.Proxy

import Distribution.Gentoo.PkgManager.Types
import Distribution.Gentoo.Types
import Output

-- | Represents possible options given on the command line
data CmdLineArgs = CmdLineArgs
    { cmdLinePkgManager :: PkgManager
    , cmdLinePretend :: Bool
    , cmdLineNoDeep :: Bool
    , cmdLineVersion :: Bool
    , cmdLineAction :: WithCmd
    , cmdLineTarget :: Either CustomTargets BuildTarget
    , cmdLineMode :: RunMode
    , cmdLineVerbosity :: Verbosity
    , cmdLineHelp :: Bool
    , cmdLineWorldFull :: Bool
    } deriving (Show, Eq, Ord)

defCmdLineArgs :: PkgManager -> CmdLineArgs
defCmdLineArgs defPM = CmdLineArgs
    defPM
    False
    False
    False
    PrintAndRun
    (Right OnlyInvalid)
    BasicMode
    Normal
    False
    False

data BuildTarget
    = OnlyInvalid -- ^ Default
    | AllInstalled -- ^ Rebuild every haskell package
    | WorldTarget -- ^ Target @world portage set
    | PreservedRebuild -- ^ Append @preserved-rebuild set
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
    argInfo PreservedRebuild =
        ( "preserved-rebuild"
        , Just $ "Append @preserved-rebuild set (only valid with\n"
              ++ "portage package manager and basic mode)"
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
        (Just s, Just d) -> case lines d of
            (l:ls) -> [paddedFst s l] ++ (paddedRest <$> ls)
            _ -> []
    paddedFst s d =
        s ++ replicate (padMax - length s) ' ' ++ " : " ++ d
    paddedRest d = replicate (padMax + 3) ' ' ++ d
    padMax = maximum $ length . snd <$> argFields
    argFields = (\a -> (a, showArg a)) <$> args
    showArg a = " * " ++ argString a ++ showDef a
    showDef a
        | optDefault (Proxy @a) == a = " (default)"
        | otherwise = ""
    args = [minBound :: a .. maxBound]

fromCmdline
    :: forall a. CmdlineOpt a
    => (a -> CmdLineArgs -> CmdLineArgs)
    -> String
    -> CmdLineArgs
    -> Either String CmdLineArgs
fromCmdline update s rm =
    case L.find (\a -> argString a == lowerS) args of
        Nothing -> Left $ "Unknown " ++ name ++ ": " ++ lowerS
        Just a -> Right $ update a rm
  where
    lowerS = map toLower s
    name = optName $ Proxy @a
    args = [minBound :: a .. maxBound]
