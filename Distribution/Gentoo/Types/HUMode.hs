
module Distribution.Gentoo.Types.HUMode
    ( HUMode(..)
    , PkgManager(..)
    , RunMode(..)
    , runMode
    , Target(..)
    , ReinstallAtomsMode(..)
    , ReinstallAtomsTarget(..)
    ) where

import Distribution.Gentoo.Types (RunModifier)

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
    deriving (Eq, Ord, Show)

runMode :: PkgManager -> Either RunMode ReinstallAtomsMode
runMode (Portage rm) = rm
runMode (PkgCore rm) = Left rm
runMode (Paludis rm) = Left rm
runMode (CustomPM _ rm) = Left rm
