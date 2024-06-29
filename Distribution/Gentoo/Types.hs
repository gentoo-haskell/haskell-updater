{-# LANGUAGE TypeApplications #-}

module Distribution.Gentoo.Types
  ( RunModifier(..)
  , RawPMArgs
  , WithCmd(..)
  , WithUserCmd
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

-- | Run-mode haskell-updater state
data RunModifier = RM { flags    :: [PMFlag]
                      , withCmd  :: WithCmd
                      , rawPMArgs :: RawPMArgs
                      , verbosity :: Verbosity
                      }
                   deriving (Eq, Ord, Show)

-- | Arguments to be passed when calling the package manager
type RawPMArgs = [String]

data WithCmd = PrintAndRun
             | PrintOnly
             | RunOnly
               deriving (Eq, Ord, Show, Read, Enum, Bounded)

type WithUserCmd = Either String WithCmd

-- | The current package list(s) organized by mode and build target
data PackageState
    = DefaultModeState (Maybe DefaultModePkgs)
    | ListModeState ListModePkgs
    | RAModeState AllPkgs RAModePkgs
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
    targetPkgs (RAModeState _ (RAModeInvalid ps)) = getPkgs ps
    targetPkgs (RAModeState ps RAModeAll) = getPkgs ps
    targetPkgs (RAModeState _ (RAModeWorld _)) = Set.empty

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
