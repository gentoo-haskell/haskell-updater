{- |
   Module      : Distribution.Gentoo.Types.HUMode
   Description : Valid modes for haskell-updater

   This module houses a complex ADT which represents valid modes for
   haskell-updater. This is converted from @CmdLineArgs@ (in
   @Distribution.Gentoo.CmdLine.Types@), which represents
   possible options given on the command line.
 -}

{-# LANGUAGE LambdaCase #-}

module Distribution.Gentoo.Types.HUMode
    ( HUMode(..)
    , PkgManager(..)
    , RunMode(..)
    , runMode
    , Target(..)
    , getTarget
    , ReinstallAtomsMode(..)
    , ReinstallAtomsTarget(..)
    , getLoopType
    , getExtraRawArgs
    ) where

import Distribution.Gentoo.Types hiding (Target)

data HUMode
    = HelpMode
    | VersionMode
    | RunMode RunModifier PkgManager
    deriving (Eq, Ord, Show)

data PkgManager
    = Portage (Either RunMode ReinstallAtomsMode)
    | PkgCore RunMode
    | Paludis RunMode
    | CustomPM String RunMode
    deriving (Eq, Ord, Show)

data RunMode
    = BasicMode Target
    | ListMode Target
    deriving (Eq, Ord, Show)

data Target
    = OnlyInvalid
    | AllInstalled
    deriving (Eq, Ord, Show)

newtype ReinstallAtomsMode
    = ReinstallAtomsMode (Either Target ReinstallAtomsTarget)
    deriving (Eq, Ord, Show)

data ReinstallAtomsTarget
    = WorldTarget
    | CustomTargets CustomTargets
    deriving (Eq, Ord, Show)

runMode :: PkgManager -> Either RunMode ReinstallAtomsMode
runMode (Portage rm) = rm
runMode (PkgCore rm) = Left rm
runMode (Paludis rm) = Left rm
runMode (CustomPM _ rm) = Left rm

getTarget :: RunMode -> Target
getTarget (BasicMode t) = t
getTarget (ListMode t) = t

-- | Convert from the @haskell-updater@ ADT to 'LoopMode', which encodes
--   how the looping mechanism of @haskell-updater@ should funciton.
--
--   Takes a 'PkgManager' as 'RunMode' is the only mode where looping makes
--   sense.
getLoopType :: PkgManager -> LoopType
getLoopType = \case
    -- Even @--mode=reinstall-atoms@ should not loop if @--target=all@ is set
    Portage (Right (ReinstallAtomsMode (Left AllInstalled))) -> NoLoop
    Portage (Right (ReinstallAtomsMode _)) -> UntilNoChange
    Portage (Left mode) -> fromRunMode mode
    PkgCore mode -> fromRunMode mode
    Paludis mode -> fromRunMode mode
    CustomPM _ mode -> fromRunMode mode
  where
    fromRunMode :: RunMode -> LoopType
    fromRunMode = \case
        BasicMode _ -> UntilNoPending
        ListMode _ -> NoLoop

-- | Convert from the @haskell-updater@ ADT to 'ExtraRawArgs', hard-coded extra
--   arguments that will be passed to the package manager.
--
--   Takes a 'PkgManager' as 'RunMode' is the only mode which runs a package
--   manager.
getExtraRawArgs :: PkgManager -> ExtraRawArgs
getExtraRawArgs _ = ExtraRawArgs [] -- currently unused feature
