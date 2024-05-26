
module Distribution.Gentoo.Types
  ( RunModifier(..)
  , WithCmd(..)
  , WithUserCmd
  , BuildTarget(..)
  , HackportMode(..)
  , PackageState(..)
  , DefaultModePkgs(..)
  , ListModePkgs(..)
  , RAModePkgs(..)
  , HasTargets(..)
  , InvalidPkgs(..)
  , AllPkgs(..)
  , PackageSet(..)
  ) where

import qualified Data.Set as Set

import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager.Types
import Output

-- | Full haskell-updater state
data RunModifier = RM { pkgmgr   :: PkgManager
                      , flags    :: [PMFlag]
                      , withCmd  :: WithCmd
                      , rawPMArgs :: [String]
                      , verbosity :: Verbosity
                      , showHelp :: Bool
                      , showVer :: Bool
                      , target   :: BuildTarget
                      , mode :: HackportMode
                      }
                   deriving (Eq, Ord, Show)

data WithCmd = PrintAndRun
             | PrintOnly
             | RunOnly
               deriving (Eq, Ord, Show, Read, Enum, Bounded)

type WithUserCmd = Either String WithCmd

data BuildTarget
    = OnlyInvalid -- ^ Default
    | AllInstalled -- ^ Rebuild every haskell package
    | WorldTarget -- ^ Target @world portage set
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

data HackportMode
    = BasicMode
    | ListMode
    | ReinstallAtomsMode
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | The current package list(s) organized by mode and build target
data PackageState
    = DefaultModeState (Maybe DefaultModePkgs)
    | ListModeState ListModePkgs
    | RAModeState (Maybe RAModePkgs)
    deriving (Show, Eq, Ord)

data DefaultModePkgs
    = DefaultInvalid InvalidPkgs
    | DefaultAll AllPkgs
    deriving (Show, Eq, Ord)

data ListModePkgs
    = ListInvalid InvalidPkgs
    | ListAll AllPkgs
    deriving (Show, Eq, Ord)

data RAModePkgs
    = RAModeInvalid AllPkgs InvalidPkgs
    | RAModeAll AllPkgs
    | RAModeWorld InvalidPkgs
    deriving (Show, Eq, Ord)

class HasTargets t where
    targets :: t -> Set.Set Package

instance HasTargets PackageState where
    targets (DefaultModeState ps) = targets ps
    targets (ListModeState ps) = targets ps
    targets (RAModeState ps) = targets ps

instance HasTargets DefaultModePkgs where
    targets (DefaultInvalid ps) = getPkgs ps
    targets (DefaultAll ps) = getPkgs ps

instance HasTargets ListModePkgs where
    targets (ListInvalid ps) = getPkgs ps
    targets (ListAll ps) = getPkgs ps

instance HasTargets RAModePkgs where
    targets (RAModeInvalid ps _) = getPkgs ps
    targets (RAModeAll ps) = getPkgs ps
    targets (RAModeWorld ps) = getPkgs ps

instance HasTargets t => HasTargets (Maybe t) where
    targets (Just ps) = targets ps
    targets Nothing = Set.empty

newtype InvalidPkgs = InvalidPkgs (Set.Set Package)
    deriving (Show, Eq, Ord)

newtype AllPkgs = AllPkgs (Set.Set Package)
    deriving (Show, Eq, Ord)

class PackageSet t where
    getPkgs :: t -> Set.Set Package

instance PackageSet InvalidPkgs where
    getPkgs (InvalidPkgs ps) = ps

instance PackageSet AllPkgs where
    getPkgs (AllPkgs ps) = ps
