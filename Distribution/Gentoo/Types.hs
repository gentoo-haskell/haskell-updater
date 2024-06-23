
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
    | RAModeState AllPkgs (Maybe RAModePkgs)
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
    = RAModeInvalid InvalidPkgs
    | RAModeAll
    | RAModeWorld InvalidPkgs
    deriving (Show, Eq, Ord)

class HasTargets t where
    targetPkgs :: t -> Set.Set Package

instance HasTargets PackageState where
    targetPkgs (DefaultModeState ps) = targetPkgs ps
    targetPkgs (ListModeState ps) = targetPkgs ps
    targetPkgs (RAModeState _ (Just (RAModeInvalid ps))) = getPkgs ps
    targetPkgs (RAModeState ps (Just RAModeAll)) = getPkgs ps
    targetPkgs (RAModeState _ (Just (RAModeWorld _))) = Set.empty
    targetPkgs (RAModeState _ Nothing) = Set.empty

instance HasTargets DefaultModePkgs where
    targetPkgs (DefaultInvalid ps) = getPkgs ps
    targetPkgs (DefaultAll ps) = getPkgs ps

instance HasTargets ListModePkgs where
    targetPkgs (ListInvalid ps) = getPkgs ps
    targetPkgs (ListAll ps) = getPkgs ps

instance HasTargets t => HasTargets (Maybe t) where
    targetPkgs (Just ps) = targetPkgs ps
    targetPkgs Nothing = Set.empty

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

instance PackageSet () where
    getPkgs () = Set.empty
